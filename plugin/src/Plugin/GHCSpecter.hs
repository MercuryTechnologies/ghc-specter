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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Maybe (isNothing)
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
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Unit.Module.Location (ModLocation (ml_hie_file))
import GHC.Unit.Module.ModSummary (ModSummary (..))
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
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    emptyConfig,
    loadConfig,
  )
import GHCSpecter.Util.GHC (showPpr)
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Console
  ( CommandSet (..),
    breakPoint,
    emptyCommandSet,
  )
import Plugin.GHCSpecter.Task.PrintCore (printCore)
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

-- | GHC session-wide initialization
initGhcSession :: [CommandLineOption] -> HscEnv -> IO MsgQueue
initGhcSession opts env = do
  ps <- atomically $ readTVar sessionRef
  let ghcSessionInfo = psSessionInfo ps
      mtimeQueue =
        (,) <$> sessionStartTime ghcSessionInfo <*> psMessageQueue ps
  case mtimeQueue of
    -- session start
    Nothing -> do
      startTime <- getCurrentTime
      pid <- fromInteger . toInteger <$> getCurrentPid
      queue <- initMsgQueue
      let modGraph = hsc_mod_graph env
          modGraphInfo = extractModuleGraphInfo modGraph
      ecfg <- loadConfig defaultGhcSpecterConfigFile
      let cfg1 =
            case ecfg of
              Left _ -> emptyConfig
              Right cfg -> cfg
          -- overwrite ipcfile if specified by CLI arguments
          cfg2 =
            case opts of
              ipcfile : _ -> cfg1 {configSocket = ipcfile}
              _ -> cfg1
          newGhcSessionInfo =
            modGraphInfo
              `seq` SessionInfo
                pid
                (Just startTime)
                modGraphInfo
                (configStartWithBreakpoint cfg2)
      atomically $
        modifyTVar'
          sessionRef
          ( \ps ->
              ps
                { psSessionConfig = cfg2
                , psSessionInfo = newGhcSessionInfo
                , psMessageQueue = Just queue
                }
          )
      void $ forkOS $ runMessageQueue cfg2 queue
      queueMessage queue (CMSession newGhcSessionInfo)
      pure queue
    -- session has started already.
    Just (_, queue) ->
      pure queue

-- | Driver session-wide initialization
initDriverSession :: IO DriverId
initDriverSession = do
  newDrvId <-
    atomically $ do
      drvId' <- psNextDriverId <$> readTVar sessionRef
      modifyTVar' sessionRef (\s -> s {psNextDriverId = drvId' + 1})
      pure drvId'
  pure newDrvId

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
typecheckPlugin queue drvId _modsummary tc = do
  let cmdSet = CommandSet [(":unqualified", fetchUnqualifiedImports tc)]
  breakPoint queue drvId Typecheck cmdSet
  pure tc

--
-- core plugin
--

corePlugin :: MsgQueue -> DriverId -> [CoreToDo] -> CoreM [CoreToDo]
corePlugin queue drvId todos = do
  dflags <- getDynFlags
  let startPlugin =
        CoreDoPluginPass
          "Core2CoreStart"
          (eachPlugin (T.pack "Core2CoreStart"))
      mkPlugin pass =
        let label = "After:" <> show pass
         in CoreDoPluginPass label (eachPlugin (T.pack label))
      todos' = startPlugin : concatMap (\todo -> [todo, mkPlugin (showPpr dflags todo)]) todos
  pure todos'
  where
    cmdSet guts = CommandSet [(":print-core", printCore guts)]

    eachPlugin pass guts = do
      _ <- printCore guts
      breakPoint queue drvId (Core2Core pass) (cmdSet guts)
      pure guts

--
-- top-level driver plugin
--

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env0 = do
  queue <- initGhcSession opts env0
  -- Module name is unknown when this driver plugin is called.
  -- Therefore, we save the module name when it is available
  -- in the actual compilation runPhase.
  modNameRef <- newIORef Nothing
  drvId <- initDriverSession
  let -- NOTE: this will wipe out all other plugins and fix opts
      -- TODO: if other plugins exist, throw exception.
      newPlugin =
        plugin
          { installCoreToDos = \_opts -> corePlugin queue drvId
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
