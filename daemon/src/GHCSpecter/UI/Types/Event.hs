module GHCSpecter.UI.Types.Event
  ( -- * Enums
    Tab (..),
    DetailLevel (..),

    -- * Event types
    SubModuleEvent (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    TimingEvent (..),
    Event (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Tab = TabSession | TabModuleGraph | TabSourceView | TabTiming
  deriving (Eq)

data DetailLevel = UpTo30 | UpTo100 | UpTo300
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DetailLevel

instance ToJSON DetailLevel

data ModuleGraphEvent
  = HoverOnModuleEv (Maybe Text)
  | ClickOnModuleEv (Maybe Text)
  | DummyEv

data SubModuleEvent
  = SubModuleGraphEv ModuleGraphEvent
  | SubModuleLevelEv DetailLevel

data SessionEvent
  = SaveSessionEv
  | ResumeSessionEv
  | PauseSessionEv

data TimingEvent
  = UpdateSticky Bool
  | UpdatePartition Bool
  | UpdateParallel Bool

data Event
  = TabEv Tab
  | ExpandModuleEv (Maybe Text)
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SessionEv SessionEvent
  | TimingEv TimingEvent
