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
import Data.Bifunctor (first, second)
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
import GHCSpecter.Channel
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

data MsgQueueState = MsgQueueState
  { msgSenderQueue :: Seq ChanMessageBox
  , msgIsPaused :: Bool
  }

emptyMsgQueueState :: MsgQueueState
emptyMsgQueueState = MsgQueueState Seq.empty False

type MsgQueue = TVar MsgQueueState

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = driver
    , typeCheckResultAction = typecheckPlugin
    }

-- | Global variable shared across the session
sessionRef :: TVar (SessionInfo, Maybe MsgQueue)
{-# NOINLINE sessionRef #-}
sessionRef =
  let newSessionState = (SessionInfo 0 Nothing emptyModuleGraphInfo False, Nothing)
   in unsafePerformIO (newTVarIO newSessionState)

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
        s <- readTVar queue
        let queued = msgSenderQueue s
        if Seq.null queued
          then retry
          else do
            writeTVar queue s {msgSenderQueue = Seq.empty}
            pure queued
      let msgList = F.toList msgs
      msgList `seq` sendObject sock msgList
    receiver :: Socket -> IO ()
    receiver sock = forever $ do
      putStrLn "################"
      putStrLn "receiver started"
      putStrLn "################"
      msg :: Bool <- receiveObject sock
      putStrLn "################"
      putStrLn $ "message received: " ++ show msg
      putStrLn "################"
      atomically $
        modifyTVar' queue $ \s -> s {msgIsPaused = msg}

queueMessage :: MsgQueue -> ChanMessage a -> IO ()
queueMessage queue !msg =
  atomically $
    modifyTVar' queue $ \s ->
      let q = msgSenderQueue s
       in s {msgSenderQueue = q |> CMBox msg}

breakPoint :: MsgQueue -> IO ()
breakPoint queue = do
  atomically $ do
    s <- readTVar queue
    STM.check (not (msgIsPaused s))

-- | Called only once for sending session information
startSession :: [CommandLineOption] -> HscEnv -> IO MsgQueue
startSession opts env = do
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  let modGraph = hsc_mod_graph env
      modGraphInfo = extractModuleGraphInfo modGraph
      startedSession =
        modGraphInfo `seq` SessionInfo pid (Just startTime) modGraphInfo False
  -- NOTE: return Nothing if session info is already initiated
  queue' <- newTVarIO emptyMsgQueueState
  (mNewStartedSession, queue, willStartMsgQueue) <-
    startedSession `seq` atomically $ do
      (SessionInfo _ msessionStart _ _, mqueue) <- readTVar sessionRef
      (queue, willStartMsgQueue) <-
        case mqueue of
          Nothing -> do
            modifyTVar' sessionRef (second (const (Just queue')))
            pure (queue', True)
          Just queue_ -> pure (queue_, False)
      case msessionStart of
        Nothing -> do
          modifyTVar' sessionRef (first (const startedSession))
          pure (Just startedSession, queue, willStartMsgQueue)
        Just _ -> pure (Nothing, queue, willStartMsgQueue)
  -- If session connection was never initiated, then make connection
  -- and start receiving message from the queue.
  when willStartMsgQueue $
    void $ forkOS $ runMessageQueue opts queue'
  for_ mNewStartedSession $ \newStartedSession ->
    queueMessage queue (CMSession newStartedSession)
  pure queue

sendModuleStart ::
  MsgQueue ->
  IORef (Maybe ModuleName) ->
  UTCTime ->
  CompPipeline (Maybe ModuleName)
sendModuleStart queue modNameRef startTime = do
  pstate <- getPipeState
  mmodName_ <- liftIO $ readIORef modNameRef
  let mmodName = getModuleNameFromPipeState pstate
  liftIO $ writeIORef modNameRef mmodName
  case (mmodName_, mmodName) of
    (Nothing, Just modName) -> do
      let timer = Timer [(TimerStart, startTime)]
      liftIO $
        queueMessage queue (CMTiming modName timer)
    _ -> pure ()
  pure mmodName

sendCompStateOnPhase ::
  MsgQueue ->
  DynFlags ->
  Maybe ModuleName ->
  PhasePlus ->
  CompPipeline ()
sendCompStateOnPhase queue dflags mmodName phase = do
  pstate <- getPipeState
  case phase of
    RealPhase StopLn -> do
      case mmodName of
        Nothing -> pure ()
        Just modName -> do
          -- send timing information
          endTime <- liftIO getCurrentTime
          let timer = Timer [(TimerEnd, endTime)]
          liftIO $
            queueMessage queue (CMTiming modName timer)
          -- send HIE file information to the daemon after compilation
          case (maybe_loc pstate, gopt Opt_WriteHie dflags) of
            (Just modLoc, True) -> do
              let hiefile = ml_hie_file modLoc
              liftIO $ do
                hiefile' <- canonicalizePath hiefile
                queueMessage queue (CMHsSource modName (HsSourceInfo hiefile'))
            _ -> pure ()
    RealPhase (As _) -> do
      case mmodName of
        Nothing -> pure ()
        Just modName -> do
          -- send timing information
          endTime <- liftIO getCurrentTime
          let timer = Timer [(TimerAs, endTime)]
          liftIO $
            queueMessage queue (CMTiming modName timer)
    HscOut _ modName0 _ -> do
      let modName = T.pack . moduleNameString $ modName0
      -- send timing information
      hscOutTime <- liftIO getCurrentTime
      let timer = Timer [(TimerHscOut, hscOutTime)]
      liftIO $
        queueMessage queue (CMTiming modName timer)
    _ -> pure ()

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env = do
  queue <- startSession opts env
  breakPoint queue
  startTime <- getCurrentTime
  -- Module name is unknown when this driver plugin is called.
  -- Therefore, we save the module name when it is available
  -- in the actual compilation runPhase.
  modNameRef <- newIORef Nothing
  let dflags = hsc_dflags env
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        -- pre phase timing
        mmodName0 <- sendModuleStart queue modNameRef startTime
        sendCompStateOnPhase queue dflags mmodName0 phase
        -- actual runPhase
        (phase', fp') <- runPhase phase fp
        -- post phase timing
        -- NOTE: we need to run sendModuleStart twice as the first one is not guaranteed
        -- to have module name information.
        mmodName1 <- sendModuleStart queue modNameRef startTime
        sendCompStateOnPhase queue dflags mmodName1 phase'
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
typecheckPlugin _opts modsummary tc = do
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

  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modsummary
  liftIO $ do
    (_, mqueue) <- atomically $ readTVar sessionRef
    for_ mqueue $ \queue ->
      queueMessage queue (CMCheckImports modName (T.pack rendered))
  pure tc
