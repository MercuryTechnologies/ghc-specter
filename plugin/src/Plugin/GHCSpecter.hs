{-# LANGUAGE BangPatterns #-}

-- This module provides the current module under compilation.
module Plugin.GHCSpecter
  ( -- * main plugin entry point

    -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,

    -- * Utilities
    getTopSortedModules,
  )
where

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.Foldable qualified as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Tuple (swap)
import GHC.Data.Graph.Directed qualified as G
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Flags (GeneralFlag (Opt_WriteHie))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Make
  ( moduleGraphNodes,
    topSortModuleGraph,
  )
import GHC.Driver.Phases (Phase (As, StopLn))
import GHC.Driver.Pipeline
  ( CompPipeline,
    PhasePlus (HscOut, RealPhase),
    PipeState (iface),
    getPipeState,
    maybe_loc,
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    PluginWithArgs (..),
    StaticPlugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session
  ( DynFlags,
    getDynFlags,
    gopt,
  )
import GHC.Plugins
  ( ModSummary,
    Name,
    localiseName,
    showSDoc,
  )
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
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import GHC.Utils.Outputable (Outputable (ppr))
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Inbound.Types (Pause (..))
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessage (..),
    ChanMessageBox (..),
    HsSourceInfo (..),
    ModuleGraphInfo (..),
    ModuleName,
    SessionInfo (..),
    Timer (..),
    TimerTag (..),
    emptyModuleGraphInfo,
  )
import GHCSpecter.Comm
  ( receiveObject,
    runClient,
    sendObject,
  )
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
import Network.Socket (Socket)
import System.Directory (canonicalizePath, doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (getCurrentPid)

data MsgQueue = MsgQueue
  { msgSenderQueue :: TVar (Seq ChanMessageBox)
  , msgReceiverQueue :: TVar Pause
  }

initMsgQueue :: IO MsgQueue
initMsgQueue = do
  sQ <- newTVarIO Seq.empty
  pauseRef <- newTVarIO (Pause False)
  pure $ MsgQueue sQ pauseRef

plugin :: Plugin
plugin = defaultPlugin {driverPlugin = driver}

data PluginSession = PluginSession
  { psSessionInfo :: SessionInfo
  , psMessageQueue :: Maybe MsgQueue
  , psNextDriverId :: DriverId
  }

emptyPluginSession :: PluginSession
emptyPluginSession = PluginSession (SessionInfo 0 Nothing emptyModuleGraphInfo False) Nothing 1

-- | Global variable shared across the session
sessionRef :: TVar PluginSession
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO emptyPluginSession)

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

getModuleNameFromPipeState :: PipeState -> Maybe ModuleName
getModuleNameFromPipeState pstate =
  let mmi = iface pstate
      mmod = fmap mi_module mmi
   in fmap (T.pack . moduleNameString . moduleName) mmod

runMessageQueue :: [CommandLineOption] -> MsgQueue -> IO ()
runMessageQueue opts queue = do
  mipcfile <-
    case opts of
      ipcfile : _ -> pure (Just ipcfile)
      [] -> do
        ecfg <- loadConfig defaultGhcSpecterConfigFile
        case ecfg of
          Left _ -> pure Nothing
          Right cfg -> pure (Just (configSocket cfg))
  for_ mipcfile $ \ipcfile -> do
    socketExists <- doesFileExist ipcfile
    when socketExists $
      runClient ipcfile $ \sock -> do
        _ <- forkIO $ receiver sock
        sender sock
  where
    sender :: Socket -> IO ()
    sender sock = forever $ do
      msgs <- atomically $ do
        queued <- readTVar (msgSenderQueue queue)
        if Seq.null queued
          then retry
          else do
            writeTVar (msgSenderQueue queue) Seq.empty
            pure queued
      let msgList = F.toList msgs
      msgList `seq` sendObject sock msgList
    receiver :: Socket -> IO ()
    receiver sock = forever $ do
      putStrLn "################"
      putStrLn "receiver started"
      putStrLn "################"
      msg :: Pause <- receiveObject sock
      putStrLn "################"
      putStrLn $ "message received: " ++ show msg
      putStrLn "################"
      atomically $
        writeTVar (msgReceiverQueue queue) msg

queueMessage :: MsgQueue -> ChanMessage a -> IO ()
queueMessage queue !msg =
  atomically $
    modifyTVar' (msgSenderQueue queue) (|> CMBox msg)

breakPoint :: MsgQueue -> IO ()
breakPoint queue = do
  atomically $ do
    p <- readTVar (msgReceiverQueue queue)
    STM.check (not (unPause p))

-- | Called only once for sending session information
startSession ::
  [CommandLineOption] ->
  HscEnv ->
  -- | (driver id, message queue)
  IO (DriverId, MsgQueue)
startSession opts env = do
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  let modGraph = hsc_mod_graph env
      modGraphInfo = extractModuleGraphInfo modGraph
      startedSession =
        modGraphInfo `seq` SessionInfo pid (Just startTime) modGraphInfo False
  -- NOTE: return Nothing if session info is already initiated
  queue' <- initMsgQueue
  (mNewStartedSession, drvId, queue, willStartMsgQueue) <-
    startedSession `seq` atomically $ do
      psession <- readTVar sessionRef
      let ghcSessionInfo = psSessionInfo psession
          msessionStart = sessionStartTime ghcSessionInfo
          mqueue = psMessageQueue psession
          drvId = psNextDriverId psession
      modifyTVar' sessionRef (\s -> s {psNextDriverId = drvId + 1})
      (queue, willStartMsgQueue) <-
        case mqueue of
          Nothing -> do
            modifyTVar' sessionRef (\s -> s {psMessageQueue = Just queue'})
            pure (queue', True)
          Just queue_ -> pure (queue_, False)
      case msessionStart of
        Nothing -> do
          modifyTVar' sessionRef (\s -> s {psSessionInfo = startedSession})
          pure (Just startedSession, drvId, queue, willStartMsgQueue)
        Just _ -> pure (Nothing, drvId, queue, willStartMsgQueue)
  -- If session connection was never initiated, then make connection
  -- and start receiving message from the queue.
  when willStartMsgQueue $
    void $ forkOS $ runMessageQueue opts queue'
  for_ mNewStartedSession $ \newStartedSession ->
    queueMessage queue (CMSession newStartedSession)
  pure (drvId, queue)

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
  IORef (Maybe ModuleName) ->
  CompPipeline (Maybe ModuleName)
sendModuleName queue drvId modNameRef = do
  pstate <- getPipeState
  mmodName_ <- liftIO $ readIORef modNameRef
  let mmodName = getModuleNameFromPipeState pstate
  liftIO $ writeIORef modNameRef mmodName
  -- run only when the value at modNameRef is changed from Nothing
  case (mmodName_, mmodName) of
    (Nothing, Just modName) -> do
      liftIO $
        queueMessage queue (CMModuleInfo drvId modName)
    _ -> pure ()
  pure mmodName

sendCompStateOnPhase ::
  MsgQueue ->
  DynFlags ->
  (DriverId, Maybe ModuleName) ->
  PhasePlus ->
  CompPipeline ()
sendCompStateOnPhase queue dflags (drvId, mmodName) phase = do
  pstate <- getPipeState
  case phase of
    RealPhase StopLn -> do
      -- send timing information
      endTime <- liftIO getCurrentTime
      let timer = Timer [(TimerEnd, endTime)]
      liftIO $
        queueMessage queue (CMTiming drvId timer)
      case mmodName of
        Nothing -> pure ()
        Just modName -> do
          -- send HIE file information to the daemon after compilation
          case (maybe_loc pstate, gopt Opt_WriteHie dflags) of
            (Just modLoc, True) -> do
              let hiefile = ml_hie_file modLoc
              liftIO $ do
                hiefile' <- canonicalizePath hiefile
                queueMessage queue (CMHsSource modName (HsSourceInfo hiefile'))
            _ -> pure ()
    RealPhase (As _) -> do
      -- send timing information
      endTime <- liftIO getCurrentTime
      let timer = Timer [(TimerAs, endTime)]
      liftIO $
        queueMessage queue (CMTiming drvId timer)
    HscOut _ _ _ -> do
      -- send timing information
      hscOutTime <- liftIO getCurrentTime
      let timer = Timer [(TimerHscOut, hscOutTime)]
      liftIO $
        queueMessage queue (CMTiming drvId timer)
    _ -> pure ()

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env0 = do
  (drvId, queue) <- startSession opts env0
  let -- NOTE: this will wipe out all other plugins and fix opts
      -- TODO: if other plugins exist, throw exception.
      -- queue is now passed to typecheckPlugin
      newPlugin = plugin {typeCheckResultAction = typecheckPlugin queue}
      env = env0 {hsc_static_plugins = [StaticPlugin (PluginWithArgs newPlugin opts)]}
  breakPoint queue
  startTime <- getCurrentTime
  sendModuleStart queue drvId startTime
  -- Module name is unknown when this driver plugin is called.
  -- Therefore, we save the module name when it is available
  -- in the actual compilation runPhase.
  modNameRef <- newIORef Nothing
  let dflags = hsc_dflags env
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        -- pre phase timing
        mmodName0 <- sendModuleName queue drvId modNameRef
        sendCompStateOnPhase queue dflags (drvId, mmodName0) phase
        -- actual runPhase
        (phase', fp') <- runPhase phase fp
        -- post phase timing
        -- NOTE: we need to run sendModuleName twice as the first one is not guaranteed
        -- to have module name information.
        -- TODO: Check whether this is still true.
        mmodName1 <- sendModuleName queue drvId modNameRef
        sendCompStateOnPhase queue dflags (drvId, mmodName1) phase'
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
  MsgQueue ->
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin queue _opts modsummary tc = do
  dflags <- getDynFlags
  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        L.foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs

      rendered =
        unlines $ do
          (modu, names) <- M.toList moduleImportMap
          let imported = fmap (formatName dflags) $ S.toList names
          [T.unpack modu, formatImportedNames imported]

      modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  liftIO $ queueMessage queue (CMCheckImports modName (T.pack rendered))
  pure tc
