module GHCSpecter.Driver.Session.Types
  ( -- * session types for daemon
    ServerSession (..),
    ClientSession (..),
  )
where

import Control.Concurrent.STM (TChan, TQueue, TVar)
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event
  ( Event,
  )

-- Session = State + Channel

data ServerSession = ServerSession
  { _ssServerStateRef :: TVar ServerState,
    _ssSubscriberSignal :: TChan Request
  }

data ClientSession = ClientSession
  { _csUIStateRef :: TVar UIState,
    -- _csPublisherState :: TChan (UIState, ServerState),
    _csPublisherEvent :: TQueue Event
  }
