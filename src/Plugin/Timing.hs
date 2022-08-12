{-# LANGUAGE BangPatterns #-}

-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
    sessionRef,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Tuple (swap)
import qualified GHC.Data.Graph.Directed as G
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Make
  ( moduleGraphNodes,
    topSortModuleGraph,
  )
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
    ModuleName,
    SessionInfo (..),
    Timer (..),
    emptyModuleGraphInfo,
    resetTimer,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (showPpr)

plugin :: Plugin
plugin =
  defaultPlugin {driverPlugin = driver}

-- shared across the session
sessionRef :: TVar SessionInfo
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO (SessionInfo Nothing emptyModuleGraphInfo))

getModuleNameText :: ModSummary -> T.Text
getModuleNameText = T.pack . moduleNameString . moduleName . ms_mod

gnode2ModSummary :: ModuleGraphNode -> Maybe ModSummary
gnode2ModSummary InstantiationNode {} = Nothing
gnode2ModSummary (ModuleNode emod) = Just (emsModSummary emod)

-- temporary function. ignore hs-boot cycles and InstantiatedUnit for now.
getTopSortedModules :: ModuleGraph -> [ModuleName]
getTopSortedModules =
  mapMaybe (fmap getModuleNameText . gnode2ModSummary)
    . concatMap G.flattenSCC
    . flip (topSortModuleGraph False) Nothing

extractModuleGraphInfo :: ModuleGraph -> ModuleGraphInfo
extractModuleGraphInfo modGraph = do
  let (graph, _) = moduleGraphNodes False (mgModSummaries' modGraph)
      vtxs = G.verticesG graph
      modNameFromVertex =
        fmap getModuleNameText . gnode2ModSummary . G.node_payload
      modNameMap =
        mapMaybe
          (\v -> (G.node_key v,) <$> modNameFromVertex v)
          vtxs
      modNameRevMap = fmap swap modNameMap
      topSorted = []  -- FOR NOW!
{-        mapMaybe
          (\n -> L.lookup n modNameRevMap)
          $ getTopSortedModules modGraph -}
      modDeps = fmap (\v -> (G.node_key v, G.node_dependencies v)) vtxs
   in ModuleGraphInfo modNameMap modDeps topSorted

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env = do
  let dflags = hsc_dflags env
  startTime <- getCurrentTime
  let modGraph = hsc_mod_graph env
      modGraphInfo = extractModuleGraphInfo modGraph
      startedSession = modGraphInfo `seq` SessionInfo (Just startTime) modGraphInfo
  -- NOTE: return Nothing if session info is already initiated
  mNewStartedSession <-
    startedSession `seq` atomically $ do
      SessionInfo msessionStart _ <- readTVar sessionRef
      case msessionStart of
        Nothing -> do
          writeTVar sessionRef startedSession
          pure (Just startedSession)
        Just _ -> pure Nothing
  for_ mNewStartedSession $ \(!newStartedSession) ->
    case opts of
      ipcfile : _ -> liftIO $ do
        socketExists <- doesFileExist ipcfile
        when socketExists $
          runClient ipcfile $ \sock ->
            sendObject sock $ CMBox (CMSession newStartedSession)
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
