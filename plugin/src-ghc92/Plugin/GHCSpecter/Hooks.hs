{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.GHCSpecter.Hooks (
  -- * utility
  getMemInfo,

  -- * send information
  sendModuleStart,
  sendModuleName,

  -- * hooks
  runRnSpliceHook',
  runMetaHook',
  runPhaseHook',
) where

import Control.Monad.Extra (ifM)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Core.Opt.Monad (getDynFlags)
import GHC.Data.IOEnv (getEnv)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Pipeline (runPhase)
import GHC.Driver.Plugins (
  PluginWithArgs (..),
  StaticPlugin (..),
 )
import GHC.Driver.Session (DynFlags)
import GHC.Hs.Extension (GhcRn)
import GHC.Stats (
  GCDetails (..),
  RTSStats (..),
  getRTSStats,
  getRTSStatsEnabled,
 )
import GHC.Tc.Gen.Splice (defaultRunMeta)
import GHC.Tc.Types (Env (..), RnM, TcM)
import GHC.Types.Meta (MetaHook, MetaRequest (..), MetaResult)
import GHC.Utils.Outputable (Outputable)
import GHCSpecter.Channel.Common.Types (DriverId (..), type ModuleName)
import GHCSpecter.Channel.Outbound.Types (
  BreakpointLoc (..),
  ChanMessage (..),
  MemInfo (..),
  Timer (..),
  TimerTag (..),
 )
import Language.Haskell.Syntax.Expr (HsSplice)
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Tasks (
  postMetaCommands,
  postPhaseCommands,
  preMetaCommands,
  prePhaseCommands,
  rnSpliceCommands,
 )
import Safe (readMay)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (getAllocationCounter)

import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Phases (Phase (As, StopLn))
import GHC.Driver.Pipeline.Monad
  ( CompPipeline,
    PhasePlus (HscOut, RealPhase),
    getPipeSession,
  )
import GHCSpecter.Util.GHC (showPpr)

data PhasePoint = PhaseStart | PhaseEnd

-- NOTE: always assume a single static plugin, the first arg = drvId
getDriverIdFromHscEnv :: HscEnv -> Maybe DriverId
getDriverIdFromHscEnv hscenv = do
  sp <- listToMaybe (hsc_static_plugins hscenv)
  arg1 <- listToMaybe (paArguments (spPlugin sp))
  DriverId <$> readMay arg1

getMemInfo :: IO (Maybe MemInfo)
getMemInfo = ifM getRTSStatsEnabled getInfo (pure Nothing)
  where
    getInfo = do
      rtsstats <- getRTSStats
      alloc <- getAllocationCounter
      let liveBytes = gcdetails_live_bytes . gc $ rtsstats
      pure $ Just $ MemInfo liveBytes alloc

sendModuleStart ::
  DriverId ->
  UTCTime ->
  Maybe MemInfo ->
  IO ()
sendModuleStart drvId startTime mmeminfo = do
  let timer = Timer [(TimerStart, (startTime, mmeminfo))]
  queueMessage (CMTiming drvId timer)

sendModuleName ::
  DriverId ->
  ModuleName ->
  Maybe FilePath ->
  IO ()
sendModuleName drvId modName msrcfile =
  queueMessage (CMModuleInfo drvId modName msrcfile)

runRnSpliceHook' :: HsSplice GhcRn -> RnM (HsSplice GhcRn)
runRnSpliceHook' splice = do
  mdrvId <- getDriverIdFromHscEnv . env_top <$> getEnv
  case mdrvId of
    Nothing -> pure splice
    Just drvId -> do
      breakPoint drvId RnSplice (rnSpliceCommands splice)
      pure splice

-- NOTE: This is a HACK. The constructors of MetaResult are deliberately
-- not exposed, and therefore, runMeta wraps runMetaHook with internal
-- case pattern match corresponding to MetaRequest.
-- We circumvent this problem by replacing the wrapper function provided
-- by MetaRequest constructors with our custom function with a breakpoint.
-- Unfortunately, the MetaRequest constructor functions are pure functions,
-- so the ugly unsafePerformIO hack is needed here.
wrapMeta ::
  (Outputable s) =>
  (s -> MetaResult) ->
  DriverId ->
  DynFlags ->
  s ->
  MetaResult
wrapMeta unMeta drvId dflags s =
  unsafePerformIO $ do
    breakPoint drvId PostRunMeta (postMetaCommands dflags s)
    pure (unMeta s)
{-# NOINLINE wrapMeta #-}

runMetaHook' :: MetaHook TcM
runMetaHook' metaReq expr = do
  mdrvId <- getDriverIdFromHscEnv . env_top <$> getEnv
  case mdrvId of
    Nothing -> defaultRunMeta metaReq expr
    Just drvId -> do
      dflags <- getDynFlags
      breakPoint drvId PreRunMeta (preMetaCommands expr)
      -- HACK: as constructors of MetaResult are not exported, this is the only way.
      let metaReq' =
            case metaReq of
              MetaE r -> MetaE (wrapMeta r drvId dflags)
              MetaP r -> MetaP (wrapMeta r drvId dflags)
              MetaT r -> MetaT (wrapMeta r drvId dflags)
              MetaD r -> MetaD (wrapMeta r drvId dflags)
              MetaAW r -> MetaAW (wrapMeta r drvId dflags)
      defaultRunMeta metaReq' expr

sendCompStateOnPhase ::
  DriverId ->
  PhasePlus ->
  PhasePoint ->
  CompPipeline ()
sendCompStateOnPhase drvId phase pt = do
  case phase of
    RealPhase StopLn -> liftIO do
      -- send timing information
      endTime <- getCurrentTime
      mmeminfo <- getMemInfo
      let timer = Timer [(TimerEnd, (endTime, mmeminfo))]
      queueMessage (CMTiming drvId timer)
    RealPhase (As _) ->
      case pt of
        PhaseStart -> liftIO $ do
          -- send timing information
          startTime <- getCurrentTime
          mmeminfo <- getMemInfo
          let timer = Timer [(TimerAs, (startTime, mmeminfo))]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    HscOut _ _ _ -> liftIO $ do
      -- send timing information
      hscOutTime <- getCurrentTime
      mmeminfo <- getMemInfo
      let timer = Timer [(TimerHscOut, (hscOutTime, mmeminfo))]
      queueMessage (CMTiming drvId timer)
    _ -> pure ()

runPhaseHook' ::
  PhasePlus ->
  FilePath ->
  CompPipeline (PhasePlus, FilePath)
runPhaseHook' phase fp = do
  hscenv <- getPipeSession
  let mdrvId = getDriverIdFromHscEnv hscenv
  case mdrvId of
    Nothing -> runPhase phase fp
    Just drvId -> do
      dflags <- getDynFlags
      -- pre phase timing
      let phaseTxt = T.pack (showPpr dflags phase)
          locPrePhase = PreRunPhase phaseTxt
      breakPoint drvId locPrePhase prePhaseCommands
      sendCompStateOnPhase drvId phase PhaseStart
      -- actual runPhase
      (phase', fp') <- runPhase phase fp
      -- post phase timing
      let phase'Txt = T.pack (showPpr dflags phase')
          locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
      breakPoint drvId locPostPhase postPhaseCommands
      sendCompStateOnPhase drvId phase' PhaseEnd
      pure (phase', fp')
