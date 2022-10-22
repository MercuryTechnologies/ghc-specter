{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

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
    stateTVar,
  )
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Map.Strict qualified as M
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
#if MIN_VERSION_ghc(9, 4, 0)
    staticPlugins,
#elif MIN_VERSION_ghc(9, 2, 0)
#endif
    type CommandLineOption,
  )
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Driver.Plugins (ParsedResult)
#elif MIN_VERSION_ghc(9, 2, 0)
import GHC.Hs (HsParsedModule)
#endif
import GHC.Driver.Session (gopt)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Tc.Types
  ( TcGblEnv (..),
    TcM,
    TcPlugin (..),
#if MIN_VERSION_ghc(9, 4, 0)
    TcPluginSolveResult(TcPluginOk),
#elif MIN_VERSION_ghc(9, 2, 0)
    TcPluginResult (TcPluginOk),
#endif
    unsafeTcPluginTcM,
  )
#if MIN_VERSION_ghc(9, 4, 0)
import GHC.Types.Unique.FM (emptyUFM)
#elif MIN_VERSION_ghc(9, 2, 0)
#endif
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
-- #if MIN_VERSION_ghc(9, 4, 0)
-- #elif MIN_VERSION_ghc(9, 2, 0)
    runPhaseHook',
-- #endif
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

-- TODO: Make the initialization work with GHCi.

-- | GHC session-wide initialization
initGhcSession :: [CommandLineOption] -> HscEnv -> IO MsgQueue
initGhcSession opts env = do
  -- NOTE: Read session and overwrite on the session should be done in a single
  -- atomic STM operation. As a consequence, unfortunately, the necessary IO
  -- action should be done outside STM beforehand, which implies the action will
  -- be done multiple times per every driver start.
  -- This is because we do not have a plugin insertion at real GHC initialization.
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  queue_ <- initMsgQueue
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
  -- read/write should be atomic inside a single STM. i.e. no interleaving
  -- IO actions are allowed.
  (isNewStart, queue) <-
    atomically $ do
      ps <- readTVar sessionRef
      let ghcSessionInfo = psSessionInfo ps
          mtimeQueue =
            (,) <$> sessionStartTime ghcSessionInfo <*> psMessageQueue ps
      case mtimeQueue of
        -- session has started already.
        Just (_, queue) -> pure (False, queue)
        -- session start
        Nothing -> do
          let modGraph = hsc_mod_graph env
              modGraphInfo = extractModuleGraphInfo modGraph
              newGhcSessionInfo =
                SessionInfo
                  { sessionProcessId = pid
                  , sessionStartTime = Just startTime
                  , sessionModuleGraph = modGraphInfo
                  , sessionModuleSources = M.empty
                  , sessionIsPaused = configStartWithBreakpoint cfg2
                  }
          modifyTVar'
            sessionRef
            ( \s ->
                s
                  { psSessionConfig = cfg2
                  , psSessionInfo = newGhcSessionInfo
                  , psMessageQueue = Just queue_
                  }
            )
          pure (True, queue_)
  when isNewStart $ do
    void $ forkOS $ runMessageQueue cfg2 queue
    let modGraph = hsc_mod_graph env
    modSources <- extractModuleSources modGraph
    sinfo <-
      atomically $
        stateTVar sessionRef $ \ps ->
          let sinfo0 = psSessionInfo ps
              sinfo = sinfo0 {sessionModuleSources = modSources}
              ps' = ps {psSessionInfo = sinfo}
           in (sinfo, ps')
    queueMessage queue (CMSession sinfo)
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
#if MIN_VERSION_ghc(9, 4, 0)
  ParsedResult ->
  Hsc ParsedResult
#elif MIN_VERSION_ghc(9, 2, 0)
  HsParsedModule ->
  Hsc HsParsedModule
#endif
parsedResultActionPlugin queue drvId modNameRef modSummary parsed = do
  let modName = getModuleName modSummary
      msrcFile = ml_hs_file $ ms_location modSummary
  liftIO $ do
    msrcFile' <- traverse canonicalizePath msrcFile
    writeIORef modNameRef (Just modName)
    sendModuleName queue drvId modName msrcFile'
  breakPoint queue drvId modNameRef ParsedResultAction parsedResultActionCommands
  pure parsed

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
#if MIN_VERSION_ghc(9, 4, 0)
    , tcPluginRewrite = \_ -> emptyUFM
#elif MIN_VERSION_ghc(9, 2, 0)
#endif
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
      splugin = StaticPlugin (PluginWithArgs newPlugin opts)
#if MIN_VERSION_ghc(9, 4, 0)
      env = env0 {hsc_plugins = (hsc_plugins env0) {staticPlugins = [splugin]}}
#elif MIN_VERSION_ghc(9, 2, 0)
      env = env0 {hsc_static_plugins = [splugin]}
#endif
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
