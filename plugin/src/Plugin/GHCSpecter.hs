{-# LANGUAGE BangPatterns #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter
  ( -- * main plugin entry point

    -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    readTVar,
  )
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Core.Opt.Monad (CoreM, CoreToDo (..), getDynFlags)
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Phases (Phase (As, StopLn))
import GHC.Driver.Pipeline
  ( CompPipeline,
    PhasePlus (HscOut, RealPhase),
    getPipeState,
    maybe_loc,
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    StaticPlugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session
  ( DynFlags,
    gopt,
  )
import GHC.Hs (HsParsedModule)
import GHC.Plugins (ModSummary)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Unit.Module.Location (ModLocation (ml_hie_file))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
    HsSourceInfo (..),
    SessionInfo (..),
    Timer (..),
    TimerTag (..),
  )
import GHCSpecter.Util.GHC (printPpr, showPpr)
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Console
  ( CommandSet (..),
    breakPoint,
    emptyCommandSet,
  )
import Plugin.GHCSpecter.Task.UnqualifiedImports (fetchUnqualifiedImports)
import Plugin.GHCSpecter.Types
  ( MsgQueue (..),
    PluginSession (..),
    initMsgQueue,
    sessionRef,
  )
import Plugin.GHCSpecter.Util
  ( extractModuleGraphInfo,
    getModuleName,
  )
import System.Directory (canonicalizePath)
import System.Process (getCurrentPid)

-- | Called only once for sending session information
startSession ::
  [CommandLineOption] ->
  HscEnv ->
  -- | (driver id, message queue)
  IO (DriverId, MsgQueue)
startSession opts env = do
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  let modGraph = hsc_mod_graph env
      modGraphInfo = extractModuleGraphInfo modGraph
      startedSession =
        modGraphInfo `seq` SessionInfo pid (Just startTime) modGraphInfo False
  -- NOTE: return Nothing if session info is already initiated
  queue' <- initMsgQueue
  (mNewStartedSession, drvId, queue, willStartMsgQueue) <-
    startedSession `seq` atomically $ do
      psession <- readTVar sessionRef
      let ghcSessionInfo = psSessionInfo psession
          msessionStart = sessionStartTime ghcSessionInfo
          mqueue = psMessageQueue psession
          drvId = psNextDriverId psession
      modifyTVar' sessionRef (\s -> s {psNextDriverId = drvId + 1})
      (queue, willStartMsgQueue) <-
        case mqueue of
          Nothing -> do
            modifyTVar' sessionRef (\s -> s {psMessageQueue = Just queue'})
            pure (queue', True)
          Just queue_ -> pure (queue_, False)
      case msessionStart of
        Nothing -> do
          modifyTVar' sessionRef (\s -> s {psSessionInfo = startedSession})
          pure (Just startedSession, drvId, queue, willStartMsgQueue)
        Just _ -> pure (Nothing, drvId, queue, willStartMsgQueue)
  -- If session connection was never initiated, then make connection
  -- and start receiving message from the queue.
  when willStartMsgQueue $
    void $ forkOS $ runMessageQueue opts queue'
  for_ mNewStartedSession $ \newStartedSession ->
    queueMessage queue (CMSession newStartedSession)
  pure (drvId, queue)

sendModuleStart ::
  MsgQueue ->
  DriverId ->
  UTCTime ->
  IO ()
sendModuleStart queue drvId startTime = do
  let timer = Timer [(TimerStart, startTime)]
  queueMessage queue (CMTiming drvId timer)

sendModuleName ::
  MsgQueue ->
  DriverId ->
  ModuleName ->
  IO ()
sendModuleName queue drvId modName =
  queueMessage queue (CMModuleInfo drvId modName)

sendCompStateOnPhase ::
  MsgQueue ->
  DynFlags ->
  DriverId ->
  PhasePlus ->
  CompPipeline ()
sendCompStateOnPhase queue dflags drvId phase = do
  pstate <- getPipeState
  case phase of
    RealPhase StopLn -> liftIO do
      -- send timing information
      endTime <- getCurrentTime
      let timer = Timer [(TimerEnd, endTime)]
      queueMessage queue (CMTiming drvId timer)
      -- send HIE file information to the daemon after compilation
      case (maybe_loc pstate, gopt Opt_WriteHie dflags) of
        (Just modLoc, True) -> do
          let hiefile = ml_hie_file modLoc
          hiefile' <- canonicalizePath hiefile
          queueMessage queue (CMHsSource drvId (HsSourceInfo hiefile'))
        _ -> pure ()
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

--
-- parsedResultAction plugin
--

parsedResultActionPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  ModSummary ->
  HsParsedModule ->
  Hsc HsParsedModule
parsedResultActionPlugin queue drvId modNameRef modSummary parsedMod = do
  let modName = getModuleName modSummary
  liftIO $ do
    writeIORef modNameRef (Just modName)
    sendModuleName queue drvId modName
  pure parsedMod

--
-- typecheck plugin
--

typecheckPlugin ::
  MsgQueue ->
  DriverId ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin queue drvId modsummary tc = do
  let cmdSet = CommandSet [(":unqualified", fetchUnqualifiedImports tc)]
  breakPoint queue drvId Typecheck cmdSet
  rendered <- fetchUnqualifiedImports tc
  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  liftIO $ queueMessage queue (CMCheckImports modName rendered)
  pure tc

--
-- core plugin
--

corePlugin :: [CoreToDo] -> CoreM [CoreToDo]
corePlugin todos = do
  dflags <- getDynFlags
  let n = length todos
  liftIO $ putStrLn $ "corePlugin n = " <> show n
  liftIO $ printPpr dflags todos
  let printMe pass guts = do
        liftIO $ putStrLn pass
        pure guts
      firstPlugin = CoreDoPluginPass "CoreStart" (printMe "CoreStart")
      mkPlugin pass =
        let label = "After:" <> show pass
         in CoreDoPluginPass label (printMe label)
      todos' = firstPlugin : concatMap (\todo -> [todo, mkPlugin (showPpr dflags todo)]) todos
  pure todos'

--
-- top-level driver plugin
--

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env0 = do
  -- Module name is unknown when this driver plugin is called.
  -- Therefore, we save the module name when it is available
  -- in the actual compilation runPhase.
  modNameRef <- newIORef Nothing
  (drvId, queue) <- startSession opts env0
  let -- NOTE: this will wipe out all other plugins and fix opts
      -- TODO: if other plugins exist, throw exception.
      newPlugin =
        plugin
          { installCoreToDos = \_opts -> corePlugin
          , parsedResultAction = \_opts -> parsedResultActionPlugin queue drvId modNameRef
          , typeCheckResultAction = \_opts -> typecheckPlugin queue drvId
          }
      env = env0 {hsc_static_plugins = [StaticPlugin (PluginWithArgs newPlugin opts)]}
  startTime <- getCurrentTime
  sendModuleStart queue drvId startTime
  breakPoint queue drvId StartDriver emptyCommandSet
  let dflags = hsc_dflags env
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        -- pre phase timing
        let locPrePhase = PreRunPhase (T.pack (showPpr dflags phase))
        breakPoint queue drvId locPrePhase emptyCommandSet
        sendCompStateOnPhase queue dflags drvId phase
        -- actual runPhase
        (phase', fp') <- runPhase phase fp
        -- post phase timing
        let locPostPhase = PostRunPhase (T.pack (showPpr dflags phase'))
        breakPoint queue drvId locPostPhase emptyCommandSet
        sendCompStateOnPhase queue dflags drvId phase'
        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- Main entry point
--

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}
