{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Driver.Session.Types (
  -- * session types for daemon
  ServerSession (..),
  HasServerSession (..),
  ClientSession (..),
  HasClientSession (..),
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
