{-# LANGUAGE ExplicitNamespaces #-}

module Plugin.GHCSpecter.Types (
  -- * Message Queue
  MsgQueue (..),
  initMsgQueue,

  -- * Console state
  ConsoleState (..),
  emptyConsoleState,

  -- * PluginSession
  PluginSession (..),
  emptyPluginSession,

  -- * global variable
  sessionRef,

  -- * utilities
  getMsgQueue,
  assignModuleToDriverId,
  assignModuleFileToDriverId,
  getModuleFromDriverId,
  getModuleFileFromDriverId,
) where

import Control.Concurrent.STM (
  STM,
  TVar,
  modifyTVar',
  newTVarIO,
  readTVar,
 )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHCSpecter.Channel.Common.Types (
  DriverId (..),
  type ModuleName,
 )
import GHCSpecter.Channel.Inbound.Types (ConsoleRequest (..))
import GHCSpecter.Channel.Outbound.Types (
  ChanMessageBox (..),
  ModuleGraphInfo (..),
  SessionInfo (..),
  emptyModuleGraphInfo,
  emptySessionInfo,
 )
import GHCSpecter.Config (Config, emptyConfig)
import GHCSpecter.Data.Map (
  BiKeyMap,
  emptyBiKeyMap,
  forwardLookup,
  insertToBiKeyMap,
 )
import System.IO.Unsafe (unsafePerformIO)

data MsgQueue = MsgQueue
  { msgSenderQueue :: TVar (Seq ChanMessageBox)
  , msgReceiverQueue :: TVar (Maybe (DriverId, ConsoleRequest))
  }

initMsgQueue :: IO MsgQueue
initMsgQueue = do
  sQ <- newTVarIO Seq.empty
  rQ <- newTVarIO Nothing
  pure $ MsgQueue sQ rQ

newtype ConsoleState = ConsoleState
  { consoleDriverInStep :: Maybe DriverId
  -- ^ DriverId in next step operation
  }

emptyConsoleState :: ConsoleState
emptyConsoleState = ConsoleState Nothing

data PluginSession = PluginSession
  { psSessionConfig :: Config
  , psSessionInfo :: SessionInfo
  , psModuleGraphInfo :: ModuleGraphInfo
  , psMessageQueue :: Maybe MsgQueue
  , psDrvIdModuleMap :: BiKeyMap DriverId ModuleName
  , psDrvIdModuleFileMap :: BiKeyMap DriverId FilePath
  , psNextDriverId :: DriverId
  , psConsoleState :: ConsoleState
  , psModuleBreakpoints :: [ModuleName]
  , psIsInGhcDebug :: Bool
  }

emptyPluginSession :: PluginSession
emptyPluginSession =
  PluginSession
    { psSessionConfig = emptyConfig
    , psSessionInfo = emptySessionInfo
    , psModuleGraphInfo = emptyModuleGraphInfo
    , psMessageQueue = Nothing
    , psDrvIdModuleMap = emptyBiKeyMap
    , psDrvIdModuleFileMap = emptyBiKeyMap
    , psNextDriverId = 1
    , psConsoleState = emptyConsoleState
    , psModuleBreakpoints = []
    , psIsInGhcDebug = False
    }

-- | Global variable shared across the session
sessionRef :: TVar PluginSession
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO emptyPluginSession)

-----------------------
-- Utility functions --
-----------------------

getMsgQueue :: STM (Maybe MsgQueue)
getMsgQueue =
  psMessageQueue <$> readTVar sessionRef

assignModuleToDriverId :: DriverId -> ModuleName -> STM ()
assignModuleToDriverId drvId modName = do
  s <- readTVar sessionRef
  let drvModMap = psDrvIdModuleMap s
      drvModMap' = insertToBiKeyMap (drvId, modName) drvModMap
  modifyTVar' sessionRef $ \s' -> s' {psDrvIdModuleMap = drvModMap'}

assignModuleFileToDriverId :: DriverId -> FilePath -> STM ()
assignModuleFileToDriverId drvId modFile = do
  s <- readTVar sessionRef
  let drvModFileMap = psDrvIdModuleFileMap s
      drvModFileMap' = insertToBiKeyMap (drvId, modFile) drvModFileMap
  modifyTVar' sessionRef $ \s' -> s' {psDrvIdModuleFileMap = drvModFileMap'}

getModuleFromDriverId :: DriverId -> STM (Maybe ModuleName)
getModuleFromDriverId drvId = do
  drvModMap <- psDrvIdModuleMap <$> readTVar sessionRef
  pure $ forwardLookup drvId drvModMap

getModuleFileFromDriverId :: DriverId -> STM (Maybe FilePath)
getModuleFileFromDriverId drvId = do
  drvModFileMap <- psDrvIdModuleFileMap <$> readTVar sessionRef
  pure $ forwardLookup drvId drvModFileMap
