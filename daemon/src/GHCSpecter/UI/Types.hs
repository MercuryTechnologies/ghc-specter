{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.UI.Types
  ( -- * Event types
    DetailLevel (..),
    SubModuleEvent (..),
    ModuleGraphEvent (..),
    TimingEvent (..),
    Event (..),

    -- * UI state
    Tab (..),
    ModuleGraphUI (..),
    HasModuleGraphUI (..),
    emptyModuleGraphUI,
    SourceViewUI (..),
    HasSourceViewUI (..),
    emptySourceViewUI,
    TimingUI,
    HasTimingUI (..),
    emptyTimingUI,
    UIState (..),
    HasUIState (..),
    emptyUIState,
  )
where

import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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

data TimingEvent
  = UpdateSticky Bool
  | UpdatePartition Bool
  | UpdateParallel Bool

data Event
  = TabEv Tab
  | ExpandModuleEv (Maybe Text)
  | MainModuleEv ModuleGraphEvent
  | SubModuleEv SubModuleEvent
  | SaveSessionEv
  | TimingEv TimingEvent

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

data TimingUI = TimingUI
  { _timingUISticky :: Bool
  -- ^ Whether the timing view is sticky to the current time or not
  , _timingUIPartition :: Bool
  -- ^ Whether each module timing is partitioned into division
  , _timingUIHowParallel :: Bool
  -- ^ Whether showing color-coded parallel processes
  }

makeClassy ''TimingUI

emptyTimingUI :: TimingUI
emptyTimingUI = TimingUI False False False

data UIState = UIState
  { _uiTab :: Tab
  -- ^ current tab
  , _uiMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , _uiSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  , _uiSourceView :: SourceViewUI
  -- ^ UI state of source view UI
  , _uiTiming :: TimingUI
  -- ^ UI state of Timing UI
  }

makeClassy ''UIState

emptyUIState :: UIState
emptyUIState =
  UIState
    { _uiTab = TabSession
    , _uiMainModuleGraph = ModuleGraphUI Nothing Nothing
    , _uiSubModuleGraph = (UpTo30, ModuleGraphUI Nothing Nothing)
    , _uiSourceView = emptySourceViewUI
    , _uiTiming = emptyTimingUI
    }
