module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    Tab (..),
    UIState (..),
    ServerState (..),
    incrementSN,

    -- * graph visualization information
    Point (..),
    Dimension (..),
    NodeLayout (..),
    EdgeLayout (..),
    GraphVisInfo (..),
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toolbox.Channel
  ( Channel,
    SessionInfo,
    Timer,
    type ModuleName,
  )

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

data NodeLayout a = NodeLayout
  { nodePayload :: a
  -- ^ information in node
  , nodePosition :: Point
  -- ^ node center position
  , nodeSize :: Dimension
  -- ^ node width and height
  }
  deriving (Show)

data EdgeLayout = EdgeLayout
  { edgeId :: Int
  -- ^ edge id from the graph layouter
  , edgeEndNodes :: (Int, Int)
  -- ^ (source node, target node)
  , edgePoints :: [Point]
  -- ^ edge start point, bend points, end point
  }
  deriving (Show)

data GraphVisInfo = GraphVisInfo
  { gviCanvasDim :: Dimension
  , gviNodes :: [NodeLayout Text]
  , gviEdges :: [EdgeLayout]
  }
  deriving (Show)

data ServerState = ServerState
  { serverMessageSN :: Int
  , serverInbox :: Inbox
  , serverSessionInfo :: SessionInfo
  , serverTiming :: Map ModuleName Timer
  , serverModuleGraph :: Maybe GraphVisInfo
  , serverModuleClustering :: [(ModuleName, [ModuleName])]
  }

incrementSN :: ServerState -> ServerState
incrementSN ss =
  ss
    { serverMessageSN = serverMessageSN ss + 1
    }
