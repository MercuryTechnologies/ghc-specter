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
  )
where

import Control.Concurrent.STM
  ( TVar,
    newTVarIO,
  )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Inbound.Types (ConsoleRequest (..))
import GHCSpecter.Channel.Outbound.Types
  ( ChanMessageBox (..),
    SessionInfo (..),
    emptyModuleGraphInfo,
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
  { psSessionInfo :: SessionInfo
  , psMessageQueue :: Maybe MsgQueue
  , psNextDriverId :: DriverId
  , psConsoleState :: ConsoleState
  }

emptyPluginSession :: PluginSession
emptyPluginSession =
  PluginSession
    { psSessionInfo = SessionInfo 0 Nothing emptyModuleGraphInfo True
    , psMessageQueue = Nothing
    , psNextDriverId = 1
    , psConsoleState = emptyConsoleState
    }

-- | Global variable shared across the session
sessionRef :: TVar PluginSession
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO emptyPluginSession)
