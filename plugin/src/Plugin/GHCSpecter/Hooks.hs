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
import GHC.Hs.Extension (GhcRn)
import GHC.Tc.Gen.Splice (defaultRunMeta)
import GHC.Tc.Types (RnM, TcM)
import GHC.Types.Meta (MetaHook)
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
  ( emptyCommandSet,
    postPhaseCommands,
    prePhaseCommands,
    rnSpliceCommands,
  )
import Plugin.GHCSpecter.Types (MsgQueue)

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

runMetaHook' ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  MetaHook TcM
runMetaHook' queue drvId modNameRef metaReq expr = do
  breakPoint queue drvId modNameRef PreRunMeta emptyCommandSet
  result <- defaultRunMeta metaReq expr
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
