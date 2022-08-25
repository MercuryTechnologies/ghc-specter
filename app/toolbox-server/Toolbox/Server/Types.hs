{-# LANGUAGE DeriveGeneric #-}

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
    transposeGraphVis,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Toolbox.Channel
  ( Channel,
    SessionInfo (..),
    Timer,
    emptyModuleGraphInfo,
    type ModuleName,
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
  | SaveSessionEv

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
    { uiTab = TabSession
    , uiModuleExpanded = Nothing
    , uiModuleHover = Nothing
    , uiModuleClick = Nothing
    }

data Point = Point
  { pointX :: Double
  , pointY :: Double
  }
  deriving (Show, Generic)

instance FromJSON Point

instance ToJSON Point

data Dimension = Dim
  { dimWidth :: Double
  , dimHeight :: Double
  }
  deriving (Show, Generic)

instance FromJSON Dimension

instance ToJSON Dimension

data NodeLayout a = NodeLayout
  { nodePayload :: a
  -- ^ information in node
  , nodePosition :: Point
  -- ^ node center position
  , nodeSize :: Dimension
  -- ^ node width and height
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (NodeLayout a)

instance ToJSON a => ToJSON (NodeLayout a)

data EdgeLayout = EdgeLayout
  { edgeId :: Int
  -- ^ edge id from the graph layouter
  , edgeEndNodes :: (Int, Int)
  -- ^ (source node, target node)
  , edgeStartEndPoints :: (Point, Point)
  -- ^ edge start point, end point
  , edgeBendPoints :: [Point]
  -- ^ edge bend points
  }
  deriving (Show, Generic)

instance FromJSON EdgeLayout

instance ToJSON EdgeLayout

data GraphVisInfo = GraphVisInfo
  { gviCanvasDim :: Dimension
  , gviNodes :: [NodeLayout (Int, Text)]
  , gviEdges :: [EdgeLayout]
  }
  deriving (Show, Generic)

instance FromJSON GraphVisInfo

instance ToJSON GraphVisInfo

-- | swap horizontal and vertical directions.
transposeGraphVis :: GraphVisInfo -> GraphVisInfo
transposeGraphVis (GraphVisInfo dim nodeLayout edgeLayout) =
  GraphVisInfo dim' nodeLayout' edgeLayout'
  where
    xposeDim (Dim w h) = Dim h w
    xposePt (Point x y) = Point y x
    xposeNode (NodeLayout p xy d) = NodeLayout p (xposePt xy) (xposeDim d)
    xposeEdge (EdgeLayout i (j, k) (start, end) pts) =
      EdgeLayout i (j, k) (xposePt start, xposePt end) (fmap xposePt pts)
    dim' = xposeDim dim
    nodeLayout' = fmap xposeNode nodeLayout
    edgeLayout' = fmap xposeEdge edgeLayout

data ServerState = ServerState
  { serverMessageSN :: Int
  , serverInbox :: Inbox
  , serverSessionInfo :: SessionInfo
  , serverTiming :: Map ModuleName Timer
  , serverModuleGraph :: Maybe GraphVisInfo
  , serverModuleClustering :: [(ModuleName, [ModuleName])]
  , serverModuleSubgraph :: [(ModuleName, GraphVisInfo)]
  }
  deriving (Show, Generic)

instance FromJSON ServerState

instance ToJSON ServerState

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
