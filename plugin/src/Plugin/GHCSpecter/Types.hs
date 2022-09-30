module Plugin.GHCSpecter.Types
  ( -- * Message Queue
    MsgQueue (..),
    initMsgQueue,

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

data PluginSession = PluginSession
  { psSessionInfo :: SessionInfo
  , psMessageQueue :: Maybe MsgQueue
  , psNextDriverId :: DriverId
  , psDriverInStep :: Maybe DriverId
  -- ^ DriverId in next step operation
  -- TODO: find a better place
  }

emptyPluginSession :: PluginSession
emptyPluginSession =
  PluginSession (SessionInfo 0 Nothing emptyModuleGraphInfo False) Nothing 1 Nothing

-- | Global variable shared across the session
sessionRef :: TVar PluginSession
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newTVarIO emptyPluginSession)
