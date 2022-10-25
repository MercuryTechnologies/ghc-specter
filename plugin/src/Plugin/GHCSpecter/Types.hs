module Plugin.GHCSpecter.Types
  ( -- * Message Queue
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
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
  )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Inbound.Types (ConsoleRequest (..))
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessageBox (..),
    SessionInfo (..),
    emptySessionInfo,
  )
import GHCSpecter.Config (Config, emptyConfig)
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
  , psMessageQueue :: Maybe MsgQueue
  , psNextDriverId :: DriverId
  , psConsoleState :: ConsoleState
  , psModuleBreakpoints :: [ModuleName]
  }

emptyPluginSession :: PluginSession
emptyPluginSession =
  PluginSession
    { psSessionConfig = emptyConfig
    , psSessionInfo = emptySessionInfo
    , psMessageQueue = Nothing
    , psNextDriverId = 1
    , psConsoleState = emptyConsoleState
    , psModuleBreakpoints = []
    }

-- | Global variable shared across the session
sessionRef :: TVar PluginSession
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO emptyPluginSession)


getMsgQueue :: IO (Maybe MsgQueue)
getMsgQueue =
  psMessageQueue <$> atomically (readTVar sessionRef)
