{-# LANGUAGE BangPatterns #-}

-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
    sessionRef,
    --
    getTopSortedModules,
  )
where

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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Tree (drawForest)
import Data.Tuple (swap)
import qualified GHC.Data.Graph.Directed as G
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Make
  ( moduleGraphNodes,
    topSortModuleGraph,
  )
import GHC.Driver.Phases (Phase (StopLn))
import GHC.Driver.Pipeline
  ( PhasePlus (RealPhase),
    PipeState (iface, maybe_loc),
    getPipeState,
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session
  ( DynFlags,
    gopt,
    initSDocContext,
  )
import GHC.Iface.Ext.Binary
  ( HieFileResult (..),
    NameCacheUpdater (NCU),
    readHieFile,
  )
import GHC.Iface.Ext.Types
  ( HieFile (..),
    getAsts,
  )
import GHC.Iface.Ext.Utils
  ( RefMap,
    generateReferencesMap,
    getEvidenceTree,
    recoverFullType,
    renderHieType,
  )
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.Unique.Supply (mkSplitUniqSupply)
import GHC.Unit.Module.Graph
  ( ModuleGraph,
    ModuleGraphNode (..),
    mgModSummaries',
  )
import GHC.Unit.Module.Location
  ( ModLocation (ml_hie_file),
  )
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
import GHC.Unit.Module.ModSummary
  ( ExtendedModSummary (..),
    ModSummary (..),
  )
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable, renderWithContext, ppr, defaultUserStyle, text)
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
import Toolbox.Util (printPpr, showPpr)

{-
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name
import Data.Tree
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
import Data.Maybe (fromJust)
import GHC.Driver.Session
import GHC.SysTools
import qualified Data.Map as M
import Data.Foldable
-}

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
      modNameMapLst =
        mapMaybe
          (\v -> (G.node_key v,) <$> modNameFromVertex v)
          vtxs
      modNameMap :: IntMap ModuleName
      modNameMap = IM.fromList modNameMapLst
      modNameRevMap :: Map ModuleName Int
      modNameRevMap = M.fromList $ fmap swap modNameMapLst
      topSorted =
        mapMaybe
          (\n -> M.lookup n modNameRevMap)
          $ getTopSortedModules modGraph
      modDeps = IM.fromList $ fmap (\v -> (G.node_key v, G.node_dependencies v)) vtxs
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
            pstate <- getPipeState
            let mmodName = fmap (T.pack . moduleNameString . moduleName . mi_module) . iface $ pstate
            -- liftIO $ print $ maybe_loc pstate
            -- liftIO $ print $ gopt Opt_WriteHie dflags
            case (maybe_loc pstate, gopt Opt_WriteHie dflags) of
              (Just modLoc, True) -> liftIO $ do
                let hiefile = ml_hie_file modLoc
                uniq_supply <- mkSplitUniqSupply 'z'
                let nc = initNameCache uniq_supply []
                hieResult <- readHieFile (NCU (\f -> pure $ snd $ f nc)) hiefile
                let hf = hie_file_result hieResult
                    asts = hie_asts hf
                    refmap = generateReferencesMap $ getAsts asts
                -- printPpr dflags asts
                printPpr dflags refmap
                explainEv dflags hf refmap
                -- printPpr dflags asts
                pure ()
              _ -> pure ()
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


{-
dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings (LlvmConfig [] [])
-}

{-
main = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  hfr <- readHieFile (NCU (\f -> pure $ snd $ f nc)) "HieQueries.hie"
  let hf = hie_file_result hfr
      refmap = generateReferencesMap $ getAsts $ hie_asts hf
  explainEv df hf refmap point
  explainEv df hf refmap point'
  return ()
-}

{-
explainEv :: DynFlags -> HieFile -> RefMap Int -> IO ()
explainEv df hf refmap = do
  putStrLn $ replicate 26 '='
  putStr $ drawForest ptrees
  where
    mtree = getEvidenceTree hf refmap

    ptrees = maybeToList (fmap (pprint . fmap expandType) <$> mtree)

    expandType = text . renderHieType df .
      flip recoverFullType (hie_types hf)

    pretty = unlines . (++["└"]) . ("┌":) . map ("│ "++) . lines

    pprint = pretty . renderWithContext (initSDocContext df sty) . ppr
    sty = defaultUserStyle
-}
