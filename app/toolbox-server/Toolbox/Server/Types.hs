module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    UIState (..),
    ServerState (..),
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toolbox.Channel (Channel, SessionInfo)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data UIState = UIState
  { uiTab :: Channel
  , uiModule :: Maybe Text
  }

data ServerState = ServerState
  { serverMessageSN :: Int
  , serverInbox :: Inbox
  , serverSessionInfo :: SessionInfo
  }
