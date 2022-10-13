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
import Data.IORef (IORef, newIORef, writeIORef)
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
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    StaticPlugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session (gopt)
import GHC.Hs (HsParsedModule)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
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
import Plugin.GHCSpecter.Task.Core2Core
  ( listCore,
    printCore,
  )
import Plugin.GHCSpecter.Task.Typecheck
  ( fetchUnqualifiedImports,
    testSourceLocation,
  )
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
          ( \s ->
              s
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
  Maybe FilePath ->
  IO ()
sendModuleName queue drvId modName msrcfile =
  queueMessage queue (CMModuleInfo drvId modName msrcfile)

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
      msrcFile = ml_hs_file $ ms_location modSummary
  liftIO $ do
    msrcFile' <- traverse canonicalizePath msrcFile
    writeIORef modNameRef (Just modName)
    sendModuleName queue drvId modName msrcFile'
  pure parsedMod

--
-- typecheck plugin
--

typecheckPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin queue drvId mmodNameRef modSummary tc = do
  -- send HIE file information to the daemon after compilation
  dflags <- getDynFlags
  let modLoc = ms_location modSummary
  when (gopt Opt_WriteHie dflags) $
    liftIO $ do
      let hiefile = ml_hie_file modLoc
      hiefile' <- canonicalizePath hiefile
      queueMessage queue (CMHsHie drvId hiefile')

  let cmdSet =
        CommandSet
          [ (":unqualified", \_ -> fetchUnqualifiedImports tc)
          , (":test", \_ -> testSourceLocation tc)
          ]
  breakPoint queue drvId mmodNameRef Typecheck cmdSet
  pure tc

--
-- core plugin
--

corePlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  [CoreToDo] ->
  CoreM [CoreToDo]
corePlugin queue drvId mmodNameRef todos = do
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
    cmdSet guts =
      CommandSet
        [ (":list-core", \_ -> listCore guts)
        , (":print-core", printCore guts)
        ]

    eachPlugin pass guts = do
      breakPoint queue drvId mmodNameRef (Core2Core pass) (cmdSet guts)
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
  -- TODO: Make (DriverId -> ModuleName) map and store it in SessionRef
  modNameRef <- newIORef Nothing
  drvId <- initDriverSession
  let -- NOTE: this will wipe out all other plugins and fix opts
      -- TODO: if other plugins exist, throw exception.
      newPlugin =
        plugin
          { installCoreToDos = \_opts -> corePlugin queue drvId modNameRef
          , parsedResultAction = \_opts -> parsedResultActionPlugin queue drvId modNameRef
          , typeCheckResultAction = \_opts -> typecheckPlugin queue drvId modNameRef
          }
      env = env0 {hsc_static_plugins = [StaticPlugin (PluginWithArgs newPlugin opts)]}
  startTime <- getCurrentTime
  sendModuleStart queue drvId startTime
  breakPoint queue drvId modNameRef StartDriver emptyCommandSet
  let dflags = hsc_dflags env
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        -- pre phase timing
        let phaseTxt = T.pack (showPpr dflags phase)
            locPrePhase = PreRunPhase phaseTxt
        breakPoint queue drvId modNameRef locPrePhase emptyCommandSet
        sendCompStateOnPhase queue drvId phase
        -- actual runPhase
        (phase', fp') <- runPhase phase fp
        -- post phase timing
        let phase'Txt = T.pack (showPpr dflags phase')
            locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
        breakPoint queue drvId modNameRef locPostPhase emptyCommandSet
        sendCompStateOnPhase queue drvId phase'
        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- Main entry point
--

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}
