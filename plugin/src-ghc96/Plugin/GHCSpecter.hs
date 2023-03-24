{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter (
  -- * main plugin entry point

  -- NOTE: The name "plugin" should be used as a GHC plugin.
  plugin,
) where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM (
  atomically,
  modifyTVar',
  readTVar,
  stateTVar,
 )
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Core.Opt.Pipeline.Types (CoreToDo (..))
import GHC.Driver.Backend (backendDescription)
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Plugins (ParsedResult, Plugin (..), PluginWithArgs (..), StaticPlugin (..), defaultPlugin, staticPlugins, type CommandLineOption)
import GHC.Driver.Session (gopt)
import GHC.Driver.Session qualified as GHC (DynFlags (..), GhcMode (..))
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.RTS.Flags (getRTSFlags)
import GHC.Tc.Types (TcGblEnv (..), TcM, TcPlugin (..), TcPluginSolveResult (TcPluginOk), unsafeTcPluginTcM)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types (
  Backend (..),
  BreakpointLoc (..),
  ChanMessage (..),
  GhcMode (..),
  ModuleGraphInfo (..),
  ProcessInfo (..),
  SessionInfo (..),
 )
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  emptyConfig,
  loadConfig,
 )
import GHCSpecter.Util.GHC (
  extractModuleGraphInfo,
  extractModuleSources,
  showPpr,
 )
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (LHsExpr)
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Hooks (
  runMetaHook',
  runPhaseHook',
  runRnSpliceHook',
 )
import Plugin.GHCSpecter.Tasks (
  core2coreCommands,
  emptyCommandSet,
  parsedResultActionCommands,
  renamedResultActionCommands,
  spliceRunActionCommands,
  typecheckResultActionCommands,
 )
import Plugin.GHCSpecter.Types (
  PluginSession (..),
  initMsgQueue,
  sessionRef,
 )
import Safe (headMay, readMay)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Process (getCurrentPid)

-- TODO: Make the initialization work with GHCi.

-- | GHC session-wide initialization
initGhcSession :: HscEnv -> IO ()
initGhcSession env = do
  -- NOTE: Read session and overwrite on the session should be done in a single
  -- atomic STM operation. As a consequence, unfortunately, the necessary IO
  -- action should be done outside STM beforehand, which implies the action will
  -- be done multiple times per every driver start.
  -- This is because we do not have a plugin insertion at real GHC initialization.
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  execPath <- getExecutablePath
  cwd <- canonicalizePath =<< getCurrentDirectory
  args <- getArgs
  rtsflags <- getRTSFlags
  -- print rtsflags
  queue_ <- initMsgQueue
  ecfg <- loadConfig defaultGhcSpecterConfigFile
  let cfg = either (const emptyConfig) id ecfg
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
          let ghcMode =
                case (GHC.ghcMode (hsc_dflags env)) of
                  GHC.CompManager -> CompManager
                  GHC.OneShot -> OneShot
                  GHC.MkDepend -> MkDepend
              backend =
                let desc = backendDescription (GHC.backend (hsc_dflags env))
                 in if
                        | desc == "native code generator" -> NCG
                        | desc == "LLVM" -> LLVM
                        | desc == "compiling via C" -> ViaC
                        --  | desc == "compiling to JavaScript" -> Javascript
                        | desc == "byte-code interpreter" -> Interpreter
                        | otherwise -> NoBackend
              modGraph = hsc_mod_graph env
              modGraphInfo = extractModuleGraphInfo modGraph
              newGhcSessionInfo =
                SessionInfo
                  { sessionProcess = Just (ProcessInfo pid execPath cwd args rtsflags)
                  , sessionGhcMode = ghcMode
                  , sessionBackend = backend
                  , sessionStartTime = Just startTime
                  , sessionModuleGraph = modGraphInfo
                  , sessionModuleSources = M.empty
                  , sessionIsPaused = configStartWithBreakpoint cfg
                  }
          modifyTVar'
            sessionRef
            ( \s ->
                s
                  { psSessionConfig = cfg
                  , psSessionInfo = newGhcSessionInfo
                  , psMessageQueue = Just queue_
                  }
            )
          pure (True, queue_)
  when isNewStart $ do
    void $ forkOS $ runMessageQueue cfg queue
    let modGraph = hsc_mod_graph env
    modSources <- extractModuleSources modGraph
    sinfo <-
      atomically $
        stateTVar sessionRef $ \ps ->
          let sinfo0 = psSessionInfo ps
              sinfo = sinfo0 {sessionModuleSources = modSources}
              ps' = ps {psSessionInfo = sinfo}
           in (sinfo, ps')
    queueMessage (CMSession sinfo)

--
-- parsedResultAction plugin
--

parsedResultActionPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  ParsedResult ->
  Hsc ParsedResult
parsedResultActionPlugin opts _ parsed = do
  for_ (DriverId <$> (readMay =<< headMay opts)) $ \drvId -> do
    breakPoint drvId ParsedResultAction parsedResultActionCommands
  pure parsed

--
-- renamedResultAction plugin
--

renamedResultActionPlugin ::
  [CommandLineOption] ->
  TcGblEnv ->
  HsGroup GhcRn ->
  TcM (TcGblEnv, HsGroup GhcRn)
renamedResultActionPlugin opts env grp = do
  for_ (DriverId <$> (readMay =<< headMay opts)) $ \drvId -> do
    breakPoint
      drvId
      RenamedResultAction
      (renamedResultActionCommands grp)
  pure (env, grp)

--
-- spliceRunAction plugin
--

spliceRunActionPlugin ::
  [CommandLineOption] ->
  LHsExpr GhcTc ->
  TcM (LHsExpr GhcTc)
spliceRunActionPlugin opts expr = do
  for_ (DriverId <$> (readMay =<< headMay opts)) $ \drvId -> do
    breakPoint
      drvId
      SpliceRunAction
      (spliceRunActionCommands expr)
  pure expr

--
-- typecheck plugin
--

typecheckPlugin ::
  [CommandLineOption] ->
  Maybe TcPlugin
typecheckPlugin opts =
  (DriverId <$> (readMay =<< headMay opts)) <&> \drvId ->
    TcPlugin
      { tcPluginInit =
          unsafeTcPluginTcM $ do
            breakPoint
              drvId
              TypecheckInit
              emptyCommandSet
            pure ()
      , tcPluginSolve = \_ _ _ _ ->
          unsafeTcPluginTcM $ do
            breakPoint
              drvId
              TypecheckSolve
              emptyCommandSet
            pure (TcPluginOk [] [])
      , tcPluginRewrite = \_ -> emptyUFM
      , tcPluginStop = \_ ->
          unsafeTcPluginTcM $ do
            breakPoint
              drvId
              TypecheckStop
              emptyCommandSet
            pure ()
      }

--
-- typeCheckResultAction plugin
--

typeCheckResultActionPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typeCheckResultActionPlugin opts modSummary tc = do
  for_ (DriverId <$> (readMay =<< headMay opts)) $ \drvId -> do
    -- send HIE file information to the daemon after compilation
    dflags <- getDynFlags
    let modLoc = ms_location modSummary
    when (gopt Opt_WriteHie dflags) $
      liftIO $ do
        let hiefile = ml_hie_file modLoc
        hiefile' <- canonicalizePath hiefile
        queueMessage (CMHsHie drvId hiefile')
    breakPoint
      drvId
      TypecheckResultAction
      (typecheckResultActionCommands tc)
  pure tc

--
-- core plugin
--

corePlugin ::
  [CommandLineOption] ->
  [CoreToDo] ->
  CoreM [CoreToDo]
corePlugin opts todos = do
  case (DriverId <$> (readMay =<< headMay opts)) of
    Nothing -> pure todos
    Just drvId -> do
      dflags <- getDynFlags
      let eachPlugin pass guts = do
            breakPoint drvId (Core2Core pass) (core2coreCommands guts)
            pure guts
          startPlugin =
            CoreDoPluginPass
              "Core2CoreStart"
              (eachPlugin (T.pack "Core2CoreStart"))
          mkPlugin pass =
            let label = "After:" <> show pass
             in CoreDoPluginPass label (eachPlugin (T.pack label))
          todos' = startPlugin : concatMap (\todo -> [todo, mkPlugin (showPpr dflags todo)]) todos
      pure todos'

--
-- top-level driver plugin
--

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env0 = do
  initGhcSession env0
  -- Note: Here we try to detect the start of the compilation of each module
  -- and assign driver id per the instance.
  -- From GHC 9.4, the start point can be consistenly detected by runPhaseHook.
  -- However, unfortunately, on GHC 9.2, runPhaseHook is not called there in
  -- the usual "ghc --make" case, but fortunately, the driver plugin initialization
  -- is invoked at the module compilation start though this is not an intended
  -- behavior. Thus, I assign the driver id here for GHC 9.2.
  -- NOTE2: this will wipe out all other plugins and fix opts
  -- TODO: if other plugins exist, throw exception.
  -- TODO: intefaceLoadAction plugin (interfere with driverPlugin due to withPlugin)
  sinfo0 <-
    atomically $
      psSessionInfo <$> readTVar sessionRef
  let modGraphInfo0 = sessionModuleGraph sinfo0
      modGraph1 = hsc_mod_graph env0
      modGraphInfo1 = extractModuleGraphInfo modGraph1
  when (IM.size (mginfoModuleNameMap modGraphInfo0) < IM.size (mginfoModuleNameMap modGraphInfo1)) $ do
    let sinfo1 = sinfo0 {sessionModuleGraph = modGraphInfo1}
    atomically $
      modifyTVar'
        sessionRef
        (\s -> s {psSessionInfo = sinfo1})
    queueMessage (CMSession sinfo1)
  let newPlugin =
        plugin
          { installCoreToDos = corePlugin
          , parsedResultAction = parsedResultActionPlugin
          , renamedResultAction = renamedResultActionPlugin
          , spliceRunAction = spliceRunActionPlugin
          , tcPlugin = typecheckPlugin
          , typeCheckResultAction = typeCheckResultActionPlugin
          }
      splugin = StaticPlugin (PluginWithArgs newPlugin opts)
      env = env0 {hsc_plugins = (hsc_plugins env0) {staticPlugins = [splugin]}}
  let hooks = hsc_hooks env
      hooks' =
        hooks
          { runRnSpliceHook = Just runRnSpliceHook'
          , runMetaHook = Just runMetaHook'
          , runPhaseHook = Just runPhaseHook'
          }
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- Main entry point
--

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}
