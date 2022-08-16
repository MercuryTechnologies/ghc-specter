module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    Tab (..),
    Event (..),
    -- * UI state
    UIState (..),
    initUIState,
    -- * Server state
    ServerState (..),
    initServerState,
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
    SessionInfo (..),
    Timer,
    type ModuleName,
    emptyModuleGraphInfo,
  )

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data Tab = TabSession | TabModuleGraph | TabCheckImports | TabTiming
  deriving (Eq)

data Event
  = TabEv Tab
  | ExpandModuleEv (Maybe Text)
  | HoverOnModuleEv (Maybe Text)
  | ClickOnModuleEv (Maybe Text)

data UIState = UIState
  { uiTab :: Tab
  -- ^ current tab
  , uiModuleExpanded :: Maybe Text
  -- ^ expanded module in CheckImports
  , uiModuleHover :: Maybe Text
  -- ^ module under mouse cursor in Module Graph
  , uiModuleClick :: Maybe Text
  -- ^ module clicked in Module Graph
  }

initUIState :: UIState
initUIState =
  UIState
    { uiTab = TabSession,
      uiModuleExpanded = Nothing,
      uiModuleHover = Nothing,
      uiModuleClick = Nothing
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
  , serverModuleSubgraph :: [(ModuleName, GraphVisInfo)]
  }

initServerState :: ServerState
initServerState =
  ServerState
    { serverMessageSN = 0
    , serverInbox = mempty
    , serverSessionInfo = SessionInfo Nothing emptyModuleGraphInfo
    , serverTiming = mempty
    , serverModuleGraph = Nothing
    , serverModuleClustering = []
    , serverModuleSubgraph = []
    }

incrementSN :: ServerState -> ServerState
incrementSN ss =
  ss
    { serverMessageSN = serverMessageSN ss + 1
    }
