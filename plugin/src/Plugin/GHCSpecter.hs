{-# LANGUAGE BangPatterns #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter
  ( -- * main plugin entry point

    -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
    -- NOTE: The name "frontendPlugin" should be used as a GHC frontend plugin.
    frontendPlugin,

    -- * Global variable for the session information
    sessionRef,

    -- * Utilities
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
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.IORef (readIORef)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Tuple (swap)
import GHC
  ( failed,
    getSessionDynFlags,
    guessTarget,
    load,
    setSessionDynFlags,
    setTargets,
  )
import GHC.Data.Graph.Directed qualified as G
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Make
  ( LoadHowMuch (LoadAllTargets),
    moduleGraphNodes,
    topSortModuleGraph,
  )
import GHC.Driver.Monad (Ghc, getSession, setSession)
import GHC.Driver.Phases
  ( Phase (StopLn),
    isHaskellishTarget,
  )
import GHC.Driver.Pipeline
  ( PhasePlus (RealPhase),
    PipeState (iface),
    compileFile,
    getPipeState,
    maybe_loc,
    runPhase,
  )
import GHC.Driver.Plugins
  ( FrontendPlugin (..),
    Plugin (..),
    defaultFrontendPlugin,
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session
  ( DynFlags (..),
    GhcMode (CompManager),
    Option (FileOption),
    getDynFlags,
    gopt,
  )
import GHC.Plugins
  ( ModSummary,
    Name,
    localiseName,
    showSDoc,
  )
import GHC.Runtime.Loader (initializePlugins)
import GHC.Tc.Types (TcGblEnv (..), TcM)
import GHC.Types.Name.Reader
  ( GlobalRdrElt (..),
    GreName (..),
    ImpDeclSpec (..),
    ImportSpec (..),
  )
import GHC.Unit.Module.Graph
  ( ModuleGraph,
    ModuleGraphNode (..),
    mgModSummaries',
  )
import GHC.Unit.Module.Location (ModLocation (ml_hie_file))
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
import GHC.Unit.Module.ModSummary
  ( ExtendedModSummary (..),
    ModSummary (..),
  )
import GHC.Unit.Module.Name (mkModuleName, moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import GHCSpecter.Channel
  ( ChanMessage (..),
    ChanMessageBox (..),
    HsSourceInfo (..),
    ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer (..),
    emptyModuleGraphInfo,
    resetTimer,
  )
import GHCSpecter.Comm (runClient, sendObject)
import GHCSpecter.Util.GHC (printPpr)
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO.Unsafe (unsafePerformIO)

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = driver
    , typeCheckResultAction = typecheckPlugin
    }

-- | Global variable shared across the session
sessionRef :: TVar SessionInfo
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO (SessionInfo Nothing emptyModuleGraphInfo))

-- | send message to the web daemon
sendMsgToDaemon :: [CommandLineOption] -> ChanMessage a -> IO ()
sendMsgToDaemon opts msg =
  case opts of
    ipcfile : _ -> liftIO $ do
      socketExists <- doesFileExist ipcfile
      when socketExists $
        runClient ipcfile $ \sock ->
          sendObject sock (CMBox msg)
    _ -> pure ()

--
-- driver plugin
--

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
  startTime <- getCurrentTime
  let dflags = hsc_dflags env
      modGraph = hsc_mod_graph env
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
        (phase', fp') <- runPhase phase fp
        case phase' of
          RealPhase StopLn -> do
            pstate <- getPipeState
            let mmi = iface pstate
                mmod = fmap mi_module mmi
                mmodName = fmap (T.pack . moduleNameString . moduleName) mmod
            -- send HIE file information to the daemon after compilation
            case (maybe_loc pstate, gopt Opt_WriteHie dflags, mmodName) of
              (Just modLoc, True, Just modName) -> do
                let hiefile = ml_hie_file modLoc
                liftIO $ do
                  hiefile' <- canonicalizePath hiefile
                  sendMsgToDaemon opts (CMHsSource modName (HsSourceInfo hiefile'))
              _ -> pure ()

            case mmodName of
              Nothing -> pure ()
              Just modName -> liftIO $ do
                endTime <- getCurrentTime
                let timer = timer0 {timerEnd = Just endTime}
                sendMsgToDaemon opts (CMTiming modName timer)
          _ -> pure ()

        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'

--
-- typechecker plugin
--

formatName :: DynFlags -> Name -> String
formatName dflags name =
  let str = showSDoc dflags . ppr . localiseName $ name
   in case str of
        (x : _) ->
          -- NOTE: As we want to have resultant text directly copied and pasted to
          --       the source code, the operator identifiers should be wrapped with
          --       parentheses.
          if isAlpha x
            then str
            else "(" ++ str ++ ")"
        _ -> str

formatImportedNames :: [String] -> String
formatImportedNames names =
  case fmap (++ ",\n") $ L.sort names of
    l0 : ls ->
      let l0' = "  ( " ++ l0
          ls' = fmap ("    " ++) ls
          footer = "  )"
       in concat ([l0'] ++ ls' ++ [footer])
    _ -> "  ()"

mkModuleNameMap :: GlobalRdrElt -> [(ModuleName, Name)]
mkModuleNameMap gre = do
  spec <- gre_imp gre
  case gre_name gre of
    NormalGreName name -> do
      let modName = T.pack . moduleNameString . is_mod . is_decl $ spec
      pure (modName, name)
    -- TODO: Handle the record field name case correctly.
    FieldGreName _ -> []

-- | First argument in -fplugin-opt is interpreted as the socket file path.
--   If nothing, do not try to communicate with web frontend.
typecheckPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin opts modsummary tc = do
  dflags <- getDynFlags
  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        L.foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs

  let rendered =
        unlines $ do
          (modu, names) <- M.toList moduleImportMap
          let imported = fmap (formatName dflags) $ S.toList names
          [T.unpack modu, formatImportedNames imported]

  liftIO $ print rendered

  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  liftIO $ sendMsgToDaemon opts (CMCheckImports modName (T.pack rendered))
  pure tc

-- front-end plugin
frontendPlugin :: FrontendPlugin
frontendPlugin =
  defaultFrontendPlugin
    { frontend = \opts srcs -> do
        liftIO $ print opts
        -- following code is copied from doMake in ghc/ghc/Main.hs
        let (hs_srcs, non_hs_srcs) = L.partition isHaskellishTarget srcs
        hsc_env <- getSession
        o_files <-
          mapM
            (\x -> liftIO $ compileFile hsc_env StopLn x)
            non_hs_srcs
        dflags <- getSessionDynFlags
        -- hijack the plugin with this module
        let thisModule = mkModuleName "Plugin.GHCSpecter"
            otherPlugins = pluginModNames dflags
            otherPluginOpts = pluginModNameOpts dflags
            dflags' =
              dflags
                { ldInputs =
                    map (FileOption "") o_files
                      ++ ldInputs dflags
                , -- frontend plugin is OneShot by default, so change it.
                  ghcMode = CompManager
                , pluginModNames = thisModule : otherPlugins
                , pluginModNameOpts = fmap (thisModule,) opts ++ otherPluginOpts
                }
        _ <- setSessionDynFlags dflags'
        targets <- mapM (uncurry guessTarget) hs_srcs
        setTargets targets
        ok_flag <- load LoadAllTargets
        when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
        pure ()
    }
