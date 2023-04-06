module GHCSpecter.UI.Types.Event (
  -- * Enums
  Tab (..),
  DetailLevel (..),
  BlockerDetailLevel (..),
  blockerThreshold,
  ComponentTag (..),
  ScrollDirection (..),

  -- * Event types
  SourceViewEvent (..),
  SubModuleEvent (..),
  ModuleGraphEvent (..),
  SessionEvent (..),
  BlockerModuleGraphEvent (..),
  TimingEvent (..),
  MouseEvent (..),
  ConsoleEvent (..),
  BackgroundEvent (..),
  Event (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)

data Tab = TabSession | TabModuleGraph | TabSourceView | TabTiming
  deriving (Eq, Show)

data DetailLevel = UpTo30 | UpTo100 | UpTo300
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DetailLevel

instance ToJSON DetailLevel

data BlockerDetailLevel = Blocking2 | Blocking3 | Blocking4 | Blocking5
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BlockerDetailLevel

instance ToJSON BlockerDetailLevel

blockerThreshold :: BlockerDetailLevel -> Int
blockerThreshold Blocking2 = 2
blockerThreshold Blocking3 = 3
blockerThreshold Blocking4 = 4
blockerThreshold Blocking5 = 5

data ComponentTag
  = TimingView
  | TimingRange
  deriving (Show, Eq)

data ScrollDirection
  = ScrollDirectionRight
  | ScrollDirectionLeft
  | ScrollDirectionDown
  | ScrollDirectionUp
  deriving (Show, Eq)

data SourceViewEvent
  = SelectModule ModuleName
  | UnselectModule
  | SetBreakpoint ModuleName Bool
  | SourceViewTab (Text, Int)
  deriving (Show, Eq)

data ModuleGraphEvent
  = HoverOnModuleEv (Maybe ModuleName)
  | ClickOnModuleEv (Maybe ModuleName)
  deriving (Show, Eq)

data SubModuleEvent
  = SubModuleGraphEv ModuleGraphEvent
  | SubModuleLevelEv DetailLevel
  deriving (Show, Eq)

data SessionEvent
  = SaveSessionEv
  | ResumeSessionEv
  | PauseSessionEv
  deriving (Show, Eq)

data BlockerModuleGraphEvent
  = BMGGraph ModuleGraphEvent
  | BMGUpdateLevel BlockerDetailLevel
  deriving (Show, Eq)

data TimingEvent
  = ToCurrentTime
  | -- | is thawed (i.e. flowing) = True, is frozen = False
    TimingFlow Bool
  | UpdatePartition Bool
  | UpdateParallel Bool
  | HoverOnModule ModuleName
  | HoverOffModule ModuleName
  | ShowBlockerGraph
  | CloseBlockerGraph
  | BlockerModuleGraphEv BlockerModuleGraphEvent
  deriving (Show, Eq)

data MouseEvent
  = MouseMove (Maybe (Double, Double))
  | MouseDown (Maybe (Double, Double))
  | MouseUp (Maybe (Double, Double))
  | Scroll ScrollDirection (Double, Double)
  | ZoomUpdate (Double, Double) Double
  | ZoomEnd
  deriving (Show, Eq)

data ConsoleEvent k
  = ConsoleTab k
  | ConsoleKey Text
  | ConsoleInput Text
  | -- | True: send the command immediately, False: wait for user's Enter.
    ConsoleButtonPressed Bool Text
  deriving (Show, Eq)

data BackgroundEvent
  = MessageChanUpdated
  | RefreshUI
  deriving (Show, Eq)

data Event
  = TabEv Tab
  | SourceViewEv SourceViewEvent
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SessionEv SessionEvent
  | TimingEv TimingEvent
  | MouseEv ComponentTag MouseEvent
  | ConsoleEv (ConsoleEvent DriverId)
  | BkgEv BackgroundEvent
  deriving (Show, Eq)
