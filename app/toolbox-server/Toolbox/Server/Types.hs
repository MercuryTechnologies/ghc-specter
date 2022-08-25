{-# LANGUAGE DeriveGeneric #-}

module Toolbox.Server.Types
  ( type ChanModule,
    type Inbox,
    Tab (..),

    -- * Event types
    DetailLevel (..),
    SubModuleEvent (..),
    ModuleGraphEvent (..),
    Event (..),

    -- * UI state
    ModuleGraphUI (..),
    UIState (..),
    emptyUIState,

    -- * ModuleGraph and Hie state
    ModuleGraphState (..),
    emptyModuleGraphState,
    RefRow' (..),
    HieState (..),
    emptyHieState,

    -- * Server state
    ServerState (..),
    emptyServerState,
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
import GHC.Types.Name (OccName, occNameString)
import HieDb.Compat (Unit)
import HieDb.Types (DeclRow (..), DefRow (..), RefRow (..))
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

data ModuleGraphEvent
  = HoverOnModuleEv (Maybe Text)
  | ClickOnModuleEv (Maybe Text)

data DetailLevel = UpTo30 | UpTo100 | UpTo300
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DetailLevel

instance ToJSON DetailLevel

data SubModuleEvent
  = SubModuleGraphEv ModuleGraphEvent
  | SubModuleLevelEv DetailLevel

data Event
  = TabEv Tab
  | ExpandModuleEv (Maybe Text)
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SaveSessionEv

data ModuleGraphUI = ModuleGraphUI
  { modGraphUIHover :: Maybe Text
  -- ^ module under mouse cursor in Module Graph
  , modGraphUIClick :: Maybe Text
  -- ^ module clicked in Module Graph
  }

data UIState = UIState
  { uiTab :: Tab
  -- ^ current tab
  , uiModuleExpanded :: Maybe Text
  -- ^ expanded module in CheckImports
  , uiMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , uiSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  }

emptyUIState :: UIState
emptyUIState =
  UIState
    { uiTab = TabSession
    , uiModuleExpanded = Nothing
    , uiMainModuleGraph = ModuleGraphUI Nothing Nothing
    , uiSubModuleGraph = (UpTo30, ModuleGraphUI Nothing Nothing)
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

data ModuleGraphState = ModuleGraphState
  { mgsClusterGraph :: Maybe GraphVisInfo
  , mgsClustering :: [(ModuleName, [ModuleName])]
  , mgsSubgraph :: [(DetailLevel, [(ModuleName, GraphVisInfo)])]
  }
  deriving (Show, Generic)

instance FromJSON ModuleGraphState

instance ToJSON ModuleGraphState

emptyModuleGraphState :: ModuleGraphState
emptyModuleGraphState = ModuleGraphState Nothing [] []

-- | RefRow has OccName which is not JSON-serializable.
data RefRow' = RefRow'
  { ref'Src :: FilePath
  , ref'NameOcc :: Text
  , ref'NameMod :: ModuleName
  , ref'NameUnit :: Text
  , ref'SLine :: Int
  , ref'SCol :: Int
  , ref'ELine :: Int
  , ref'ECol :: Int
  }
  deriving (Show, Generic)

instance FromJSON RefRow'

instance ToJSON RefRow'

newtype HieState = HieState
  { hieModuleMap :: [(ModuleName, [RefRow'])]  -- , [DeclRow], [DefRow]))]
  }
  deriving (Show, Generic)

instance FromJSON HieState

instance ToJSON HieState

emptyHieState :: HieState
emptyHieState = HieState []

data ServerState = ServerState
  { serverMessageSN :: Int
  , serverInbox :: Inbox
  , serverSessionInfo :: SessionInfo
  , serverTiming :: Map ModuleName Timer
  , serverModuleGraphState :: ModuleGraphState
  , serverHieState :: HieState
  }
  deriving (Show, Generic)

instance FromJSON ServerState

instance ToJSON ServerState

emptyServerState :: ServerState
emptyServerState =
  ServerState
    { serverMessageSN = 0
    , serverInbox = mempty
    , serverSessionInfo = SessionInfo Nothing emptyModuleGraphInfo
    , serverTiming = mempty
    , serverModuleGraphState = emptyModuleGraphState
    , serverHieState = emptyHieState
    }

incrementSN :: ServerState -> ServerState
incrementSN ss =
  ss
    { serverMessageSN = serverMessageSN ss + 1
    }
