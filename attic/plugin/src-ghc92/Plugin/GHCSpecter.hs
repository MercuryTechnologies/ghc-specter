{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter
  ( -- * main plugin entry point

    -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    readTVar,
  )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
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
import GHC.Tc.Types (TcGblEnv (..), TcM, TcPlugin (..), TcPluginResult (TcPluginOk), unsafeTcPluginTcM)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
  )
import GHCSpecter.Util.GHC (getModuleName, showPpr)
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (LHsExpr)
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Hooks
  ( getMemInfo,
    runMetaHook',
    runPhaseHook',
    runRnSpliceHook',
    sendModuleName,
    sendModuleStart,
  )
import Plugin.GHCSpecter.Init (initGhcSession)
import Plugin.GHCSpecter.Tasks (core2coreCommands, driverCommands, emptyCommandSet, parsedResultActionCommands, renamedResultActionCommands, spliceRunActionCommands, typecheckResultActionCommands)
import Plugin.GHCSpecter.Types
  ( PluginSession (..),
    assignModuleFileToDriverId,
    assignModuleToDriverId,
    sessionRef,
  )
import Safe (headMay, readMay)
import System.Directory (canonicalizePath)
import System.Mem (setAllocationCounter)

-- | Driver session-wide initialization
initDriverSession :: IO DriverId
initDriverSession = do
  newDrvId <-
    atomically $ do
      drvId' <- psNextDriverId <$> readTVar sessionRef
      modifyTVar' sessionRef (\s -> s {psNextDriverId = drvId' + 1})
      pure drvId'
  pure newDrvId

--
-- parsedResultAction plugin
--

parsedResultActionPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  HsParsedModule ->
  Hsc HsParsedModule
parsedResultActionPlugin opts modSummary parsed = do
  for_ (DriverId <$> (readMay =<< headMay opts)) $ \drvId -> do
    -- NOTE: on GHC 9.2, we send module name information here.
    let modName = getModuleName modSummary
        msrcFile = ml_hs_file $ ms_location modSummary
    liftIO $ do
      msrcFile' <- traverse canonicalizePath msrcFile
      atomically $ assignModuleToDriverId drvId modName
      for_ msrcFile $ \srcFile ->
        atomically $ assignModuleFileToDriverId drvId srcFile
      sendModuleName drvId modName msrcFile'
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
            pure (),
        tcPluginSolve = \_ _ _ _ ->
          unsafeTcPluginTcM $ do
            breakPoint
              drvId
              TypecheckSolve
              emptyCommandSet
            pure (TcPluginOk [] []),
        tcPluginStop = \_ ->
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
driver _opts env0 = do
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
  drvId <- initDriverSession
  let opts' = [show (unDriverId drvId)] -- ignore opts
      newPlugin =
        plugin
          { installCoreToDos = corePlugin,
            parsedResultAction = parsedResultActionPlugin,
            renamedResultAction = renamedResultActionPlugin,
            spliceRunAction = spliceRunActionPlugin,
            tcPlugin = typecheckPlugin,
            typeCheckResultAction = typeCheckResultActionPlugin
          }
      splugin = StaticPlugin (PluginWithArgs newPlugin opts')
      env = env0 {hsc_static_plugins = [splugin]}
  -- send module start signal here on GHC 9.2
  startTime <- getCurrentTime
  setAllocationCounter 0
  mmeminfo <- getMemInfo
  sendModuleStart drvId startTime mmeminfo
  breakPoint drvId StartDriver driverCommands
  let hooks = hsc_hooks env
      hooks' =
        hooks
          { runRnSpliceHook = Just runRnSpliceHook',
            runMetaHook = Just runMetaHook',
            runPhaseHook = Just runPhaseHook'
          }
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- Main entry point
--

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}
