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
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    StaticPlugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session (gopt)
import GHC.Hs (HsParsedModule)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Tc.Types
  ( TcGblEnv (..),
    TcM,
    TcPlugin (..),
    TcPluginResult (..),
    unsafeTcPluginTcM,
  )
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
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (LHsExpr)
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Hooks
  ( runMetaHook',
    runPhaseHook',
    runRnSpliceHook',
  )
import Plugin.GHCSpecter.Tasks
  ( core2coreCommands,
    driverCommands,
    emptyCommandSet,
    parsedResultActionCommands,
    renamedResultActionCommands,
    spliceRunActionCommands,
    typecheckResultActionCommands,
  )
import Plugin.GHCSpecter.Types
  ( MsgQueue (..),
    PluginSession (..),
    initMsgQueue,
    sessionRef,
  )
import Plugin.GHCSpecter.Util
  ( extractModuleGraphInfo,
    extractModuleSources,
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
          modSources = extractModuleSources modGraph
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
            SessionInfo
              { sessionProcessId = pid
              , sessionStartTime = Just startTime
              , sessionModuleGraph = modGraphInfo
              , sessionModuleSources = modSources
              , sessionIsPaused = configStartWithBreakpoint cfg2
              }
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
  breakPoint queue drvId modNameRef ParsedResultAction parsedResultActionCommands
  pure parsedMod

--
-- renamedResultAction plugin
--

renamedResultActionPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  TcGblEnv ->
  HsGroup GhcRn ->
  TcM (TcGblEnv, HsGroup GhcRn)
renamedResultActionPlugin queue drvId modNameRef env grp = do
  breakPoint
    queue
    drvId
    modNameRef
    RenamedResultAction
    (renamedResultActionCommands grp)
  pure (env, grp)

--
-- spliceRunAction plugin
--

spliceRunActionPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  LHsExpr GhcTc ->
  TcM (LHsExpr GhcTc)
spliceRunActionPlugin queue drvId modNameRef expr = do
  breakPoint
    queue
    drvId
    modNameRef
    SpliceRunAction
    (spliceRunActionCommands expr)
  pure expr

--
-- typecheck plugin
--

typecheckPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  TcPlugin
typecheckPlugin queue drvId modNameRef =
  TcPlugin
    { tcPluginInit =
        unsafeTcPluginTcM $ do
          breakPoint
            queue
            drvId
            modNameRef
            TypecheckInit
            emptyCommandSet
          pure ()
    , tcPluginSolve = \_ _ _ _ ->
        unsafeTcPluginTcM $ do
          breakPoint
            queue
            drvId
            modNameRef
            TypecheckSolve
            emptyCommandSet
          pure (TcPluginOk [] [])
    , tcPluginStop = \_ ->
        unsafeTcPluginTcM $ do
          breakPoint
            queue
            drvId
            modNameRef
            TypecheckStop
            emptyCommandSet
          pure ()
    }

--
-- typeCheckResultAction plugin
--

typeCheckResultActionPlugin ::
  MsgQueue ->
  DriverId ->
  IORef (Maybe ModuleName) ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typeCheckResultActionPlugin queue drvId modNameRef modSummary tc = do
  -- send HIE file information to the daemon after compilation
  dflags <- getDynFlags
  let modLoc = ms_location modSummary
  when (gopt Opt_WriteHie dflags) $
    liftIO $ do
      let hiefile = ml_hie_file modLoc
      hiefile' <- canonicalizePath hiefile
      queueMessage queue (CMHsHie drvId hiefile')
  breakPoint
    queue
    drvId
    modNameRef
    TypecheckResultAction
    (typecheckResultActionCommands tc)
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
    eachPlugin pass guts = do
      breakPoint queue drvId mmodNameRef (Core2Core pass) (core2coreCommands guts)
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
      -- TODO: intefaceLoadAction plugin (interfere with driverPlugin due to withPlugin)
      -- TODO: tcPlugin. need different mechanism
      newPlugin =
        plugin
          { installCoreToDos = \_opts -> corePlugin queue drvId modNameRef
          , parsedResultAction = \_opts -> parsedResultActionPlugin queue drvId modNameRef
          , renamedResultAction = \_opts -> renamedResultActionPlugin queue drvId modNameRef
          , spliceRunAction = \_opts -> spliceRunActionPlugin queue drvId modNameRef
          , tcPlugin = \_opts -> Just $ typecheckPlugin queue drvId modNameRef
          , typeCheckResultAction = \_opts -> typeCheckResultActionPlugin queue drvId modNameRef
          }
      env = env0 {hsc_static_plugins = [StaticPlugin (PluginWithArgs newPlugin opts)]}
  startTime <- getCurrentTime
  sendModuleStart queue drvId startTime
  breakPoint queue drvId modNameRef StartDriver driverCommands
  let hooks = hsc_hooks env
      hooks' =
        hooks
          { runRnSpliceHook = Just (runRnSpliceHook' queue drvId modNameRef)
          , runMetaHook = Just (runMetaHook' queue drvId modNameRef)
          , runPhaseHook = Just (runPhaseHook' queue drvId modNameRef)
          }
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- Main entry point
--

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}
