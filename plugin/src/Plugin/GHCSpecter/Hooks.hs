{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

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
import GHC.Driver.Pipeline (runPhase)
#if MIN_VERSION_ghc(9, 4, 0)
import Data.Text (Text)
import GHC.Driver.Pipeline.Phases
  ( PhaseHook (..),
    TPhase (..),
  )
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Pipeline (CompPipeline, PhasePlus (HscOut, RealPhase))
#endif
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

data PhasePoint = PhaseStart | PhaseEnd

runRnSpliceHook' ::
  DriverId ->
  IORef (Maybe ModuleName) ->
  HsSplice GhcRn ->
  RnM (HsSplice GhcRn)
runRnSpliceHook' drvId modNameRef splice = do
  breakPoint drvId modNameRef RnSplice (rnSpliceCommands splice)
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
  IORef (Maybe ModuleName) ->
  DynFlags ->
  s ->
  MetaResult
wrapMeta unMeta drvId modNameRef dflags s =
  unsafePerformIO $ do
    breakPoint drvId modNameRef PostRunMeta (postMetaCommands dflags s)
    pure (unMeta s)
{-# NOINLINE wrapMeta #-}

runMetaHook' ::
  DriverId ->
  IORef (Maybe ModuleName) ->
  MetaHook TcM
runMetaHook' drvId modNameRef metaReq expr = do
  dflags <- getDynFlags
  breakPoint drvId modNameRef PreRunMeta (preMetaCommands expr)
  -- HACK: as constructors of MetaResult are not exported, this is the only way.
  let metaReq' =
        case metaReq of
          MetaE r -> MetaE (wrapMeta r drvId modNameRef dflags)
          MetaP r -> MetaP (wrapMeta r drvId modNameRef dflags)
          MetaT r -> MetaT (wrapMeta r drvId modNameRef dflags)
          MetaD r -> MetaD (wrapMeta r drvId modNameRef dflags)
          MetaAW r -> MetaAW (wrapMeta r drvId modNameRef dflags)
  defaultRunMeta metaReq' expr

#if MIN_VERSION_ghc(9, 4, 0)
tphase2Text :: TPhase res -> Text
tphase2Text p =
  case p of
    T_Unlit {} -> "T_Unlit"
    T_FileArgs {} -> "T_FileArgs"
    T_Cpp {} -> "T_Cpp"
    T_HsPp {} -> "T_HsPp"
    T_HscRecomp {} -> "T_HsRecomp"
    T_Hsc {} -> "T_Hsc"
    T_HscPostTc {} -> "T_HscPostTc"
    T_HscBackend {} -> "T_HscBackend"
    T_CmmCpp {} -> "T_CmmCpp"
    T_Cmm {} -> "T_Cmm"
    T_Cc {} -> "T_Cc"
    T_As {} -> "T_As"
    T_LlvmOpt {} -> "T_LlvmOpt"
    T_LlvmLlc {} -> "T_LlvmLlc"
    T_LlvmMangle {} -> "T_LlvmMangle"
    T_MergeForeign {} -> "T_MergeForeign"

-- NOTE: I tried to approximate GHC 9.2 version of this function.
-- TODO: Figure out more robust way for detecting the end of
-- compilation. In the worst case, we can just turn on timing
-- log and parse the message on the fly.
sendCompStateOnPhase ::
  DriverId ->
  TPhase r ->
  PhasePoint ->
  IO ()
sendCompStateOnPhase drvId phase pt = do
  case phase of
    T_Unlit {} -> pure ()
    T_FileArgs {} -> pure ()
    T_Cpp {} -> pure ()
    T_HsPp {} -> pure ()
    T_HscRecomp {} -> pure ()
    T_Hsc {} ->
      case pt of
        PhaseStart -> do
          -- send timing information
          startTime <- getCurrentTime
          let timer = Timer [(TimerStart, startTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    T_HscPostTc {} ->
      case pt of
        PhaseEnd -> do
          -- send timing information
          hscOutTime <- getCurrentTime
          let timer = Timer [(TimerHscOut, hscOutTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    T_HscBackend {} -> pure ()
    T_CmmCpp {} -> pure ()
    T_Cmm {} -> pure ()
    T_Cc {} -> pure ()
    T_As {} ->
      case pt of
        PhaseStart -> do
          -- send timing information
          asStartTime <- getCurrentTime
          let timer = Timer [(TimerAs, asStartTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    T_LlvmOpt {} -> pure ()
    T_LlvmLlc {} -> pure ()
    T_LlvmMangle {} -> pure ()
    T_MergeForeign {} ->
      case pt of
        PhaseEnd -> do
          -- send timing information
          endTime <- getCurrentTime
          let timer = Timer [(TimerEnd, endTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()

runPhaseHook' ::
  DriverId ->
  IORef (Maybe ModuleName) ->
  PhaseHook
runPhaseHook' drvId modNameRef = PhaseHook $ \phase -> do
  let phaseTxt = tphase2Text phase
  let locPrePhase = PreRunPhase phaseTxt
  breakPoint drvId modNameRef locPrePhase prePhaseCommands
  sendCompStateOnPhase drvId phase PhaseStart
  result <- runPhase phase
  let phase'Txt = phaseTxt
      locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
  breakPoint drvId modNameRef locPostPhase postPhaseCommands
  sendCompStateOnPhase drvId phase PhaseEnd
  pure result

#elif MIN_VERSION_ghc(9, 2, 0)
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
      let timer = Timer [(TimerEnd, endTime)]
      queueMessage (CMTiming drvId timer)
    RealPhase (As _) ->
      case pt of
        PhaseStart -> liftIO $ do
          -- send timing information
          endTime <- getCurrentTime
          let timer = Timer [(TimerAs, endTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    HscOut _ _ _ -> liftIO $ do
      -- send timing information
      hscOutTime <- getCurrentTime
      let timer = Timer [(TimerHscOut, hscOutTime)]
      queueMessage (CMTiming drvId timer)
    _ -> pure ()

runPhaseHook' ::
  DriverId ->
  IORef (Maybe ModuleName) ->
  PhasePlus ->
  FilePath ->
  CompPipeline (PhasePlus, FilePath)
runPhaseHook' drvId modNameRef phase fp = do
  dflags <- getDynFlags
  -- pre phase timing
  let phaseTxt = T.pack (showPpr dflags phase)
      locPrePhase = PreRunPhase phaseTxt
  breakPoint drvId modNameRef locPrePhase prePhaseCommands
  sendCompStateOnPhase drvId phase PhaseStart
  -- actual runPhase
  (phase', fp') <- runPhase phase fp
  -- post phase timing
  let phase'Txt = T.pack (showPpr dflags phase')
      locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
  breakPoint drvId modNameRef locPostPhase postPhaseCommands
  sendCompStateOnPhase drvId phase' PhaseEnd
  pure (phase', fp')
#endif
