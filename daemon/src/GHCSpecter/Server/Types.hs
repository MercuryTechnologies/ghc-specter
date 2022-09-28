{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Server.Types
  ( type ChanModule,
    type Inbox,

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
  )
where

import Control.Lens (makeClassy, (%~))
import Data.Aeson (FromJSON, ToJSON)
-- import Data.IntMap (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Tree (Forest)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types
  ( DriverId,
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( Channel,
    SessionInfo (..),
    Timer,
    emptyModuleGraphInfo,
  )
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.UI.Types.Event (DetailLevel)
import GHCSpecter.Util.Map (BiKeyMap, KeyMap, emptyBiKeyMap, emptyKeyMap)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data ModuleGraphState = ModuleGraphState
  { _mgsModuleForest :: Forest ModuleName
  , _mgsClusterGraph :: Maybe GraphVisInfo
  , _mgsClustering :: [(ModuleName, [ModuleName])]
  , _mgsSubgraph :: [(DetailLevel, [(ModuleName, GraphVisInfo)])]
  }
  deriving (Show, Generic)

makeClassy ''ModuleGraphState

instance FromJSON ModuleGraphState

instance ToJSON ModuleGraphState

emptyModuleGraphState :: ModuleGraphState
emptyModuleGraphState = ModuleGraphState [] Nothing [] []

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

data HieState = HieState
  { _hieModuleMap :: Map ModuleName ModuleHieInfo
  , _hieCallGraphMap :: Map ModuleName GraphVisInfo
  }
  deriving (Show, Generic)

makeClassy ''HieState

instance FromJSON HieState

instance ToJSON HieState

emptyHieState :: HieState
emptyHieState = HieState mempty mempty

data ServerState = ServerState
  { _serverMessageSN :: Int
  , _serverShouldUpdate :: Bool
  , _serverInbox :: Inbox
  , _serverSessionInfo :: SessionInfo
  , _serverDriverModuleMap :: BiKeyMap DriverId ModuleName
  , _serverTiming :: KeyMap DriverId Timer
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
    , _serverShouldUpdate = True
    , _serverInbox = mempty
    , _serverSessionInfo = SessionInfo 0 Nothing emptyModuleGraphInfo False
    , _serverDriverModuleMap = emptyBiKeyMap
    , _serverTiming = emptyKeyMap
    , _serverModuleGraphState = emptyModuleGraphState
    , _serverHieState = emptyHieState
    }

incrementSN :: ServerState -> ServerState
incrementSN = serverMessageSN %~ (+ 1)
