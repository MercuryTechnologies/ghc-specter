module Plugin.GHCSpecter.Hooks
  ( runRnSpliceHook',
    runMetaHook',
    runPhaseHook',
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import GHC.Core.Opt.Monad (getDynFlags)
import GHC.Driver.Phases (Phase (As, StopLn))
import GHC.Driver.Pipeline
  ( CompPipeline,
    PhasePlus (HscOut, RealPhase),
    runPhase,
  )
import GHC.Driver.Session (DynFlags)
import GHC.Hs.Extension (GhcRn)
import GHC.Tc.Gen.Splice (defaultRunMeta)
import GHC.Tc.Types (RnM, TcM)
import GHC.Types.Meta (MetaHook, MetaRequest (..), MetaResult)
import GHC.Utils.Outputable (Outputable)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
    Timer (..),
    TimerTag (..),
  )
import GHCSpecter.Util.GHC (showPpr)
import Language.Haskell.Syntax.Expr (HsSplice)
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Tasks
  ( postMetaCommands,
    postPhaseCommands,
    preMetaCommands,
    prePhaseCommands,
    rnSpliceCommands,
  )
import Plugin.GHCSpecter.Types (MsgQueue)
import System.IO.Unsafe (unsafePerformIO)

sendCompStateOnPhase ::
  MsgQueue ->
  DriverId ->
  PhasePlus ->
  CompPipeline ()
sendCompStateOnPhase queue drvId phase = do
  case phase of
    RealPhase StopLn -> liftIO do
      -- send timing information
      endTime <- getCurrentTime
      let timer = Timer [(TimerEnd, endTime)]
      queueMessage queue (CMTiming drvId timer)
    RealPhase (As _) -> liftIO $ do
      -- send timing information
      endTime <- getCurrentTime
      let timer = Timer [(TimerAs, endTime)]
      queueMessage queue (CMTiming drvId timer)
    HscOut _ _ _ -> liftIO $ do
      -- send timing information
      hscOutTime <- getCurrentTime
      let timer = Timer [(TimerHscOut, hscOutTime)]
      queueMessage queue (CMTiming drvId timer)
    _ -> pure ()

runRnSpliceHook' ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  HsSplice GhcRn ->
  RnM (HsSplice GhcRn)
runRnSpliceHook' queue drvId modNameRef splice = do
  breakPoint queue drvId modNameRef RnSplice (rnSpliceCommands splice)
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
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  DynFlags ->
  s ->
  MetaResult
wrapMeta unMeta queue drvId modNameRef dflags s =
  unsafePerformIO $ do
    breakPoint queue drvId modNameRef PostRunMeta (postMetaCommands dflags s)
    pure (unMeta s)
{-# NOINLINE wrapMeta #-}

runMetaHook' ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  MetaHook TcM
runMetaHook' queue drvId modNameRef metaReq expr = do
  dflags <- getDynFlags
  breakPoint queue drvId modNameRef PreRunMeta (preMetaCommands expr)
  -- HACK: as constructors of MetaResult are not exported, this is the only way.
  let metaReq' =
        case metaReq of
          MetaE r -> MetaE (wrapMeta r queue drvId modNameRef dflags)
          MetaP r -> MetaP (wrapMeta r queue drvId modNameRef dflags)
          MetaT r -> MetaT (wrapMeta r queue drvId modNameRef dflags)
          MetaD r -> MetaD (wrapMeta r queue drvId modNameRef dflags)
          MetaAW r -> MetaAW (wrapMeta r queue drvId modNameRef dflags)
  defaultRunMeta metaReq' expr

runPhaseHook' ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  PhasePlus ->
  FilePath ->
  CompPipeline (PhasePlus, FilePath)
runPhaseHook' queue drvId modNameRef phase fp = do
  dflags <- getDynFlags
  -- pre phase timing
  let phaseTxt = T.pack (showPpr dflags phase)
      locPrePhase = PreRunPhase phaseTxt
  breakPoint queue drvId modNameRef locPrePhase prePhaseCommands
  sendCompStateOnPhase queue drvId phase
  -- actual runPhase
  (phase', fp') <- runPhase phase fp
  -- post phase timing
  let phase'Txt = T.pack (showPpr dflags phase')
      locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
  breakPoint queue drvId modNameRef locPostPhase postPhaseCommands
  sendCompStateOnPhase queue drvId phase'
  pure (phase', fp')
