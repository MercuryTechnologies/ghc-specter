{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Server.Types
  ( type ChanModule,
    type Inbox,

    -- * Timing state
    TimingState (..),
    HasTimingState (..),
    emptyTimingState,

    -- * ModuleGraph state
    ModuleGraphState (..),
    HasModuleGraphState (..),
    emptyModuleGraphState,

    -- * Hie state
    HieState (..),
    HasHieState (..),
    emptyHieState,
    ConsoleItem (..),

    -- * Server state
    ServerState (..),
    HasServerState (..),
    emptyServerState,
    incrementSN,
  )
where

import Control.Lens (makeClassy, (%~))
import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Tree (Forest, Tree)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types
  ( DriverId,
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc,
    Channel,
    SessionInfo (..),
    Timer,
    emptyModuleGraphInfo,
  )
import GHCSpecter.Data.GHC.Hie (ModuleHieInfo)
import GHCSpecter.Data.Timing.Types (TimingTable, emptyTimingTable)
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.UI.Types.Event (DetailLevel)
import GHCSpecter.Util.Map (BiKeyMap, KeyMap, emptyBiKeyMap, emptyKeyMap)

type ChanModule = (Channel, Text)

type Inbox = Map ChanModule Text

data TimingState = TimingState
  { _tsTimingMap :: KeyMap DriverId Timer
  , -- TODO1: This cached state (TimingTable) should be separated out
    -- as we do not want to serialize this.
    -- TODO2: The name TimingTable is rather confusing. choose different one.
    _tsTimingTable :: TimingTable
  , _tsBlockerGraph :: IntMap [Int]
  , _tsBlockerGraphViz :: Maybe GraphVisInfo
  }
  deriving (Show, Generic)

makeClassy ''TimingState

instance FromJSON TimingState

instance ToJSON TimingState

emptyTimingState :: TimingState
emptyTimingState =
  TimingState
    { _tsTimingMap = emptyKeyMap
    , _tsTimingTable = emptyTimingTable
    , _tsBlockerGraph = IM.empty
    , _tsBlockerGraphViz = Nothing
    }

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

data ConsoleItem
  = -- | Command input
    ConsoleCommand Text
  | -- | Simple text message
    ConsoleText Text
  | -- | Collection of buttons. Note that the information from ConsoleReply is
    -- enriched with the corresponding console command for convenience.
    -- Each item in the outer list is a row of buttons and ones in the inner list
    -- are (button label, console command)
    ConsoleButton [[(Text, Text)]]
  | -- | GHC Core expression trees (as untyped)
    ConsoleCore [Tree (Text, Text)]
  deriving (Show, Generic)

instance FromJSON ConsoleItem

instance ToJSON ConsoleItem

data ServerState = ServerState
  { _serverMessageSN :: Int
  , _serverShouldUpdate :: Bool
  , _serverInbox :: Inbox
  , _serverSessionInfo :: SessionInfo
  , _serverDriverModuleMap :: BiKeyMap DriverId ModuleName
  , _serverTiming :: TimingState
  , _serverPaused :: KeyMap DriverId BreakpointLoc
  , _serverConsole :: KeyMap DriverId [ConsoleItem]
  , _serverModuleGraphState :: ModuleGraphState
  , _serverHieState :: HieState
  , _serverModuleBreakpoints :: [ModuleName]
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
    , _serverTiming = emptyTimingState
    , _serverPaused = emptyKeyMap
    , _serverConsole = emptyKeyMap
    , _serverModuleGraphState = emptyModuleGraphState
    , _serverHieState = emptyHieState
    , _serverModuleBreakpoints = []
    }

incrementSN :: ServerState -> ServerState
incrementSN = serverMessageSN %~ (+ 1)
