{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter
  ( -- * main plugin entry point

    -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Text qualified as T
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Core.Opt.Pipeline.Types (CoreToDo (..))
import GHC.Driver.Env (Hsc, HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Plugins (ParsedResult, Plugin (..), PluginWithArgs (..), StaticPlugin (..), defaultPlugin, staticPlugins, type CommandLineOption)
import GHC.Driver.Session (gopt)
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Tc.Types
  ( TcGblEnv (..),
    TcM,
    TcPlugin (..),
    TcPluginSolveResult (TcPluginOk),
    unsafeTcPluginTcM,
  )
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
  )
import GHCSpecter.Util.GHC (showPpr)
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (LHsExpr)
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Hooks
  ( runMetaHook',
    runPhaseHook',
    runRnSpliceHook',
  )
import Plugin.GHCSpecter.Init (initGhcSession)
import Plugin.GHCSpecter.Tasks
  ( core2coreCommands,
    emptyCommandSet,
    parsedResultActionCommands,
    renamedResultActionCommands,
    spliceRunActionCommands,
    typecheckResultActionCommands,
  )
import Safe (headMay, readMay)
import System.Directory (canonicalizePath)

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
            pure (),
        tcPluginSolve = \_ _ _ _ ->
          unsafeTcPluginTcM $ do
            breakPoint
              drvId
              TypecheckSolve
              emptyCommandSet
            pure (TcPluginOk [] []),
        tcPluginRewrite = \_ -> emptyUFM,
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
  -- NOTE: this will wipe out all other plugins and fix opts
  -- TODO: if other plugins exist, throw exception.
  -- TODO: intefaceLoadAction plugin (interfere with driverPlugin due to withPlugin)
  let newPlugin =
        plugin
          { installCoreToDos = corePlugin,
            parsedResultAction = parsedResultActionPlugin,
            renamedResultAction = renamedResultActionPlugin,
            spliceRunAction = spliceRunActionPlugin,
            tcPlugin = typecheckPlugin,
            typeCheckResultAction = typeCheckResultActionPlugin
          }
      splugin = StaticPlugin (PluginWithArgs newPlugin opts)
      env = env0 {hsc_plugins = (hsc_plugins env0) {staticPlugins = [splugin]}}
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
