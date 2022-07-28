module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    Tab (..),
    UIState (..),
    ServerState (..),
    incrementSN,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toolbox.Channel (Channel, SessionInfo, Timer)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data Tab = TabSession | TabCheckImports | TabTiming
  deriving (Eq)

data UIState = UIState
  { uiTab :: Tab
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
