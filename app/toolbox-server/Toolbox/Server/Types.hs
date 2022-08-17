module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    Tab (..),
    UIState (..),
    Point (..),
    Dimension (..),
    GraphVisInfo (..),
    ServerState (..),
    incrementSN,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toolbox.Channel (Channel, SessionInfo, Timer)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data Tab = TabSession | TabModuleGraph | TabCheckImports | TabTiming
  deriving (Eq)

data UIState = UIState
  { uiTab :: Tab
  , uiModule :: Maybe Text
  }

data Point = Point
  { pointX :: Double
  , pointY :: Double
  }
  deriving (Show)

data Dimension = Dim
  { dimWidth :: Double
  , dimHeight :: Double
  }
  deriving (Show)

data GraphVisInfo = GraphVisInfo
  { gviCanvasDim :: Dimension
  , gviNodes :: [(Text, Point, Dimension)]
  -- ^ (module name, node position, node size)
  , gviEdges :: [(Int, [Point])]
  -- ^ (edge id, edge points)
  }
  deriving (Show)

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
