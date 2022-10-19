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
import GHC.Hs.Extension (GhcPs, GhcRn)
import GHC.Tc.Gen.Splice (defaultRunMeta)
import GHC.Tc.Types (RnM, TcM)
import GHC.ThToHs
  ( convertToHsDecls,
    convertToHsExpr,
    convertToHsType,
    convertToPat,
  )
import GHC.Types.Basic (Origin)
import GHC.Types.Meta (MetaHook, MetaRequest (..), MetaResult)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Utils.Outputable (Outputable (ppr))
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
import GHCSpecter.Util.GHC (printPpr, showPpr)
import GHCi.RemoteTypes (ForeignHValue)
import Language.Haskell.Syntax.Expr (HsSplice, LHsExpr)
import Language.Haskell.TH qualified as TH
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Tasks
  ( emptyCommandSet,
    postPhaseCommands,
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

-- HACK: as constructors of MetaResult are not exported, this is the only way.
wrapMetaE :: DynFlags -> (LHsExpr GhcPs -> MetaResult) -> LHsExpr GhcPs -> MetaResult
wrapMetaE dflags unMetaE expr = unsafePerformIO (printPpr dflags expr >> pure (unMetaE expr))
{-# NOINLINE wrapMetaE #-}

runMetaHook' ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  MetaHook TcM
runMetaHook' queue drvId modNameRef metaReq expr = do
  dflags <- getDynFlags
  breakPoint queue drvId modNameRef PreRunMeta emptyCommandSet
  -- HACK: as constructors of MetaResult are not exported, this is the only way.
  case metaReq of
    MetaE r -> do
      let r' = wrapMetaE dflags r
      result <- defaultRunMeta (MetaE r') expr
      breakPoint queue drvId modNameRef PostRunMeta emptyCommandSet
      pure result
    MetaP r -> do
      result <- defaultRunMeta (MetaP r) expr
      breakPoint queue drvId modNameRef PostRunMeta emptyCommandSet
      pure result
    MetaT r -> do
      result <- defaultRunMeta (MetaT r) expr
      breakPoint queue drvId modNameRef PostRunMeta emptyCommandSet
      pure result
    MetaD r -> do
      result <- defaultRunMeta (MetaD r) expr
      breakPoint queue drvId modNameRef PostRunMeta emptyCommandSet
      pure result
    MetaAW r -> do
      result <- defaultRunMeta (MetaAW r) expr
      breakPoint queue drvId modNameRef PostRunMeta emptyCommandSet
      pure result

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
