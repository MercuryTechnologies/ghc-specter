module GHCSpecter.UI.Types.Event
  ( -- * Enums
    Tab (..),
    DetailLevel (..),
    ComponentTag (..),

    -- * Event types
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
import GHCSpecter.Channel.Common.Types (DriverId)

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

data ModuleGraphEvent
  = HoverOnModuleEv (Maybe Text)
  | ClickOnModuleEv (Maybe Text)
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
  = UpdateSticky Bool
  | UpdatePartition Bool
  | UpdateParallel Bool
  deriving (Show, Eq)

data MouseEvent
  = MouseMove (Maybe (Double, Double))
  | MouseDown (Maybe (Double, Double))
  | MouseUp (Maybe (Double, Double))
  deriving (Show, Eq)

data ConsoleEvent
  = ConsoleTab DriverId
  | ConsoleKey Text
  | ConsoleInput Text
  deriving (Show, Eq)

data BackgroundEvent
  = MessageChanUpdated
  | RefreshUI
  deriving (Show, Eq)

data Event
  = TabEv Tab
  | ExpandModuleEv (Maybe Text)
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SessionEv SessionEvent
  | TimingEv TimingEvent
  | MouseEv ComponentTag MouseEvent
  | ConsoleEv ConsoleEvent
  | BkgEv BackgroundEvent
  deriving (Show, Eq)
