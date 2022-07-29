module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    UIState (..),
    ServerState (..),
    incrementSN,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Toolbox.Channel (Channel, SessionInfo, Timer)

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
  , serverTiming :: Map Text Timer
  }

incrementSN :: ServerState -> ServerState
incrementSN ss =
  ss
    { serverMessageSN = serverMessageSN ss + 1
    }
