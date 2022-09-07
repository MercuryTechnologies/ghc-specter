{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Server.Types
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
    HasModuleGraphUI (..),
    emptyModuleGraphUI,
    SourceViewUI (..),
    HasSourceViewUI (..),
    emptySourceViewUI,
    UIState (..),
    HasUIState (..),
    emptyUIState,

    -- * ModuleGraph state
    ModuleGraphState (..),
    HasModuleGraphState (..),
    emptyModuleGraphState,

    -- * Hie state
    RefRow' (..),
    HasRefRow' (..),
    DeclRow' (..),
    HasDeclRow' (..),
    DefRow' (..),
    HasDefRow' (..),
    ModuleHieInfo (..),
    HasModuleHieInfo (..),
    emptyModuleHieInfo,
    HieState (..),
    HasHieState (..),
    emptyHieState,

    -- * Server state
    ServerState (..),
    HasServerState (..),
    emptyServerState,
    incrementSN,

    -- * graph visualization information
    Point (..),
    HasPoint (..),
    Dimension (..),
    HasDimension (..),
    NodeLayout (..),
    HasNodeLayout (..),
    EdgeLayout (..),
    HasEdgeLayout (..),
    GraphVisInfo (..),
    HasGraphVisInfo (..),
    transposeGraphVis,
  )
where

import Control.Lens (makeClassy, (%~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel
  ( Channel,
    SessionInfo (..),
    Timer,
    emptyModuleGraphInfo,
    type ModuleName,
  )

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data Tab = TabSession | TabModuleGraph | TabSourceView | TabTiming
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
  { _modGraphUIHover :: Maybe Text
  -- ^ module under mouse cursor in Module Graph
  , _modGraphUIClick :: Maybe Text
  -- ^ module clicked in Module Graph
  }

makeClassy ''ModuleGraphUI

emptyModuleGraphUI :: ModuleGraphUI
emptyModuleGraphUI = ModuleGraphUI Nothing Nothing

newtype SourceViewUI = SourceViewUI
  { _srcViewExpandedModule :: Maybe Text
  -- ^ expanded module in CheckImports
  }

makeClassy ''SourceViewUI

emptySourceViewUI :: SourceViewUI
emptySourceViewUI = SourceViewUI Nothing

data UIState = UIState
  { _uiTab :: Tab
  -- ^ current tab
  , _uiMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , _uiSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  , _uiSourceView :: SourceViewUI
  -- ^ UI state of source view UI
  }

makeClassy ''UIState

emptyUIState :: UIState
emptyUIState =
  UIState
    { _uiTab = TabSession
    , _uiMainModuleGraph = ModuleGraphUI Nothing Nothing
    , _uiSubModuleGraph = (UpTo30, ModuleGraphUI Nothing Nothing)
    , _uiSourceView = emptySourceViewUI
    }

data Point = Point
  { _pointX :: Double
  , _pointY :: Double
  }
  deriving (Show, Generic)

makeClassy ''Point

instance FromJSON Point

instance ToJSON Point

data Dimension = Dim
  { _dimWidth :: Double
  , _dimHeight :: Double
  }
  deriving (Show, Generic)

makeClassy ''Dimension

instance FromJSON Dimension

instance ToJSON Dimension

data NodeLayout a = NodeLayout
  { _nodePayload :: a
  -- ^ information in node
  , _nodePosition :: Point
  -- ^ node center position
  , _nodeSize :: Dimension
  -- ^ node width and height
  }
  deriving (Show, Generic)

makeClassy ''NodeLayout

instance FromJSON a => FromJSON (NodeLayout a)

instance ToJSON a => ToJSON (NodeLayout a)

data EdgeLayout = EdgeLayout
  { _edgeId :: Int
  -- ^ edge id from the graph layouter
  , _edgeEndNodes :: (Int, Int)
  -- ^ (source node, target node)
  , _edgeStartEndPoints :: (Point, Point)
  -- ^ edge start point, end point
  , _edgeBendPoints :: [Point]
  -- ^ edge bend points
  }
  deriving (Show, Generic)

makeClassy ''EdgeLayout

instance FromJSON EdgeLayout

instance ToJSON EdgeLayout

data GraphVisInfo = GraphVisInfo
  { _gviCanvasDim :: Dimension
  , _gviNodes :: [NodeLayout (Int, Text)]
  , _gviEdges :: [EdgeLayout]
  }
  deriving (Show, Generic)

makeClassy ''GraphVisInfo

instance FromJSON GraphVisInfo

instance ToJSON GraphVisInfo

-- | swap horizontal and vertical directions.
transposeGraphVis :: GraphVisInfo -> GraphVisInfo
transposeGraphVis (GraphVisInfo dim nodLayout edgLayout) =
  GraphVisInfo dim' nodLayout' edgLayout'
  where
    xposeDim (Dim w h) = Dim h w
    xposePt (Point x y) = Point y x
    xposeNode (NodeLayout p xy d) = NodeLayout p (xposePt xy) (xposeDim d)
    xposeEdge (EdgeLayout i (j, k) (start, end) pts) =
      EdgeLayout i (j, k) (xposePt start, xposePt end) (fmap xposePt pts)
    dim' = xposeDim dim
    nodLayout' = fmap xposeNode nodLayout
    edgLayout' = fmap xposeEdge edgLayout

data ModuleGraphState = ModuleGraphState
  { _mgsClusterGraph :: Maybe GraphVisInfo
  , _mgsClustering :: [(ModuleName, [ModuleName])]
  , _mgsSubgraph :: [(DetailLevel, [(ModuleName, GraphVisInfo)])]
  }
  deriving (Show, Generic)

makeClassy ''ModuleGraphState

instance FromJSON ModuleGraphState

instance ToJSON ModuleGraphState

emptyModuleGraphState :: ModuleGraphState
emptyModuleGraphState = ModuleGraphState Nothing [] []

-- | RefRow has OccName and Unit which are not JSON-serializable.
data RefRow' = RefRow'
  { _ref'Src :: FilePath
  , _ref'NameOcc :: Text
  , _ref'NameMod :: ModuleName
  , _ref'NameUnit :: Text
  , _ref'SLine :: Int
  , _ref'SCol :: Int
  , _ref'ELine :: Int
  , _ref'ECol :: Int
  }
  deriving (Show, Generic)

makeClassy ''RefRow'

instance FromJSON RefRow'

instance ToJSON RefRow'

-- | DeclRow has OccName
data DeclRow' = DeclRow'
  { _decl'Src :: FilePath
  , _decl'NameOcc :: Text
  , _decl'SLine :: Int
  , _decl'SCol :: Int
  , _decl'ELine :: Int
  , _decl'ECol :: Int
  , _decl'Root :: Bool
  }
  deriving (Show, Generic)

makeClassy ''DeclRow'

instance FromJSON DeclRow'

instance ToJSON DeclRow'

-- | DefRow has OccName
data DefRow' = DefRow'
  { _def'Src :: FilePath
  , _def'NameOcc :: Text
  , _def'SLine :: Int
  , _def'SCol :: Int
  , _def'ELine :: Int
  , _def'ECol :: Int
  }
  deriving (Show, Generic)

makeClassy ''DefRow'

instance FromJSON DefRow'

instance ToJSON DefRow'

data ModuleHieInfo = ModuleHieInfo
  { _modHieRefs :: [RefRow']
  , _modHieDecls :: [DeclRow']
  , _modHieDefs :: [DefRow']
  , _modHieSource :: Text
  }
  deriving (Show, Generic)

makeClassy ''ModuleHieInfo

instance FromJSON ModuleHieInfo

instance ToJSON ModuleHieInfo

emptyModuleHieInfo :: ModuleHieInfo
emptyModuleHieInfo = ModuleHieInfo [] [] [] ""

newtype HieState = HieState
  { _hieModuleMap :: Map ModuleName ModuleHieInfo
  }
  deriving (Show, Generic)

makeClassy ''HieState

instance FromJSON HieState

instance ToJSON HieState

emptyHieState :: HieState
emptyHieState = HieState mempty

data ServerState = ServerState
  { _serverMessageSN :: Int
  , _serverInbox :: Inbox
  , _serverSessionInfo :: SessionInfo
  , _serverTiming :: Map ModuleName Timer
  , _serverModuleGraphState :: ModuleGraphState
  , _serverHieState :: HieState
  }
  deriving (Show, Generic)

makeClassy ''ServerState

instance FromJSON ServerState

instance ToJSON ServerState

emptyServerState :: ServerState
emptyServerState =
  ServerState
    { _serverMessageSN = 0
    , _serverInbox = mempty
    , _serverSessionInfo = SessionInfo Nothing emptyModuleGraphInfo
    , _serverTiming = mempty
    , _serverModuleGraphState = emptyModuleGraphState
    , _serverHieState = emptyHieState
    }

incrementSN :: ServerState -> ServerState
incrementSN = serverMessageSN %~ (+ 1)