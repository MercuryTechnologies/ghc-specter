{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver.Session.Types (
  -- * session types for daemon
  ServerSession (..),
  HasServerSession (..),
  ClientSession (..),
  HasClientSession (..),

  -- * for web
  ClientSessionWeb (..),
  HasClientSessionWeb (..),
  UIChannelWeb (..),
  HasUIChannelWeb (..),
) where

import Control.Concurrent.STM (TChan, TQueue, TVar)
import Control.Lens (makeClassy)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (
  Event,
 )

-- Session = State + Channel

data ServerSession = ServerSession
  { _ssServerStateRef :: TVar ServerState
  , _ssSubscriberSignal :: TChan Request
  }

makeClassy ''ServerSession

data ClientSession = ClientSession
  { _csUIStateRef :: TVar UIState
  , _csPublisherState :: TChan (UIState, ServerState)
  , _csPublisherEvent :: TQueue Event
  }

makeClassy ''ClientSession

--
-- the following is only needed for web (concur-replica)
--

-- TODO: move this to web

data ClientSessionWeb = ClientSessionWeb
  { _csWebUIStateRef :: TVar UIState
  , _csWebSubscriberEvent :: TChan Event
  , _csWebPublisherState :: TChan (UIState, ServerState)
  , _csWebPublisherEvent :: TQueue Event
  }

makeClassy ''ClientSessionWeb

-- | communication channel that UI renderer needs
-- Note that subscribe/publish is named according to UI side semantics.
data UIChannelWeb = UIChannelWeb
  { uiWebPublisherEvent :: TChan Event
  -- ^ channel for sending event to control
  , uiWebSubscriberState :: TChan (UIState, ServerState)
  -- ^ channel for receiving state from control
  , uiWebSubscriberEvent :: TQueue Event
  -- ^ channel for receiving background event
  }

makeClassy ''UIChannelWeb
