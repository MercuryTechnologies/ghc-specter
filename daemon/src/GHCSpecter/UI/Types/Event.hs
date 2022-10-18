module GHCSpecter.UI.Types.Event
  ( -- * Enums
    Tab (..),
    DetailLevel (..),
    ComponentTag (..),

    -- * Event types
    SourceViewEvent (..),
    SubModuleEvent (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    TimingEvent (..),
    MouseEvent (..),
    ConsoleEvent (..),
    BackgroundEvent (..),
    Event (..),
  )
where

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

data ComponentTag
  = TimingView
  | TimingBar
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
  | BlockerModuleGraphEv ModuleGraphEvent
  deriving (Show, Eq)

data MouseEvent
  = MouseMove (Maybe (Double, Double))
  | MouseDown (Maybe (Double, Double))
  | MouseUp (Maybe (Double, Double))
  deriving (Show, Eq)

data ConsoleEvent k
  = ConsoleTab k
  | ConsoleKey Text
  | ConsoleInput Text
  | ConsoleButtonPressed Text
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
