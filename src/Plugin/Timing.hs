-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
    sessionRef,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified GHC.Data.Graph.Directed as G
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Make (moduleGraphNodes)
import GHC.Driver.Phases (Phase (StopLn))
import GHC.Driver.Pipeline
  ( PhasePlus (RealPhase),
    PipeState (iface),
    getPipeState,
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Unit.Module.Graph
  ( ModuleGraph,
    ModuleGraphNode (..),
    mgModSummaries',
  )
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
import GHC.Unit.Module.ModSummary
  ( ExtendedModSummary (..),
    ModSummary (..),
  )
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Toolbox.Channel
  ( ChanMessage (CMSession, CMTiming),
    ChanMessageBox (..),
    ModuleGraphInfo (..),
    SessionInfo (..),
    Timer (..),
    resetTimer,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (showPpr)

plugin :: Plugin
plugin =
  defaultPlugin {driverPlugin = driver}

-- shared across the session
sessionRef :: IORef SessionInfo
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newIORef (SessionInfo Nothing ""))

getModuleNameText :: ModSummary -> T.Text
getModuleNameText = T.pack . moduleNameString . moduleName . ms_mod

inspectModuleGraph :: ModuleGraph -> ModuleGraphInfo
inspectModuleGraph modGraph = do
  let (graph, _) = moduleGraphNodes False (mgModSummaries' modGraph)
      gnode2ModSummary InstantiationNode {} = Nothing
      gnode2ModSummary (ModuleNode emod) = Just (emsModSummary emod)
      vtxs = G.verticesG graph
      modNameFromVertex = fmap getModuleNameText . gnode2ModSummary . G.node_payload
      modNameMap =
        mapMaybe
          (\v -> (G.node_key v,) <$> modNameFromVertex v)
          vtxs
      modDeps = fmap (\v -> (G.node_key v, G.node_dependencies v)) vtxs
   in ModuleGraphInfo modNameMap modDeps

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env = do
  let dflags = hsc_dflags env
  startTime <- getCurrentTime
  SessionInfo msessionStart _ <- readIORef sessionRef
  case msessionStart of
    Nothing -> do
      let modGraph = hsc_mod_graph env
          modGraphTxt = T.intercalate "\n" $ fmap (T.pack . showPpr dflags) $ mgModSummaries' modGraph
          startedSession = SessionInfo (Just startTime) modGraphTxt
          modGraphInfo = inspectModuleGraph modGraph
      print modGraphInfo
      writeIORef sessionRef startedSession
      case opts of
        ipcfile : _ -> liftIO $ do
          socketExists <- doesFileExist ipcfile
          when socketExists $
            runClient ipcfile $ \sock ->
              sendObject sock $ CMBox (CMSession startedSession)
        _ -> pure ()
    _ -> pure ()
  let timer0 = resetTimer {timerStart = Just startTime}
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        liftIO $ do
          putStrLn $ "###########" <> showPpr dflags phase
        (phase', fp') <- runPhase phase fp
        liftIO $ putStrLn $ "------------>" <> showPpr dflags phase'

        case phase' of
          RealPhase StopLn -> do
            mmodName <-
              fmap (T.pack . moduleNameString . moduleName . mi_module) . iface
                <$> getPipeState
            case mmodName of
              Nothing -> pure ()
              Just modName -> liftIO $ do
                endTime <- getCurrentTime
                let timer = timer0 {timerEnd = Just endTime}
                case opts of
                  ipcfile : _ -> liftIO $ do
                    socketExists <- doesFileExist ipcfile
                    when socketExists $
                      runClient ipcfile $ \sock ->
                        sendObject sock $ CMBox (CMTiming modName timer)
                  _ -> pure ()
          _ -> pure ()

        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'
