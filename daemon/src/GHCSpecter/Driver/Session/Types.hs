{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver.Session.Types (
  -- * session types for daemon
  ServerSession (..),
  HasServerSession (..),
  ClientSession (..),
  HasClientSession (..),

  -- * UI Channel
  UIChannel (..),
  HasUIChannel (..),
) where

import Control.Concurrent.STM (TChan, TQueue, TVar)
import Control.Lens (makeClassy)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Event)

-- Session = State + Channel

data ServerSession = ServerSession
  { _ssServerStateRef :: TVar ServerState
  , _ssSubscriberSignal :: TChan Request
  }

makeClassy ''ServerSession

data ClientSession = ClientSession
  { _csUIStateRef :: TVar UIState
  , _csSubscriberEvent :: TChan Event
  , _csPublisherState :: TChan (UIState, ServerState)
  , _csPublisherEvent :: TQueue Event
  }

makeClassy ''ClientSession

-- | communication channel that UI renderer needs
-- Note that subscribe/publish is named according to UI side semantics.
data UIChannel = UIChannel
  { uiPublisherEvent :: TChan Event
  -- ^ channel for sending event to control
  , uiSubscriberState :: TChan (UIState, ServerState)
  -- ^ channel for receiving state from control
  , uiSubscriberEvent :: TQueue Event
  -- ^ channel for receiving background event
  }

makeClassy ''UIChannel
