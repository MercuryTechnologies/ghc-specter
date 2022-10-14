{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.UI.Types
  ( ModuleGraphUI (..),
    HasModuleGraphUI (..),
    emptyModuleGraphUI,
    SourceViewUI (..),
    HasSourceViewUI (..),
    emptySourceViewUI,
    TimingUI,
    HasTimingUI (..),
    emptyTimingUI,
    ConsoleUI,
    HasConsoleUI (..),
    emptyConsoleUI,
    MainView (..),
    HasMainView (..),
    emptyMainView,
    UIModel (..),
    HasUIModel (..),
    emptyUIModel,
    UIView (..),
    HasUIView (..),
    UIState (..),
    HasUIState (..),
    emptyUIState,
  )
where

import Control.Lens (makeClassy)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Data.Assets (Assets)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.UI.Types.Event (DetailLevel (..), Tab (..))

data ModuleGraphUI = ModuleGraphUI
  { _modGraphUIHover :: Maybe Text
  -- ^ module under mouse cursor in Module Graph
  , _modGraphUIClick :: Maybe Text
  -- ^ module clicked in Module Graph
  }

makeClassy ''ModuleGraphUI

emptyModuleGraphUI :: ModuleGraphUI
emptyModuleGraphUI = ModuleGraphUI Nothing Nothing

data SourceViewUI = SourceViewUI
  { _srcViewExpandedModule :: Maybe Text
  -- ^ expanded module in SourceView
  , _srcViewFocusedBinding :: Maybe Text
  -- ^ focused binding if exist
  }

makeClassy ''SourceViewUI

emptySourceViewUI :: SourceViewUI
emptySourceViewUI = SourceViewUI Nothing Nothing

data TimingUI = TimingUI
  { _timingFrozenTable :: Maybe TimingTable
  -- ^ When freezing timing flow, this holds the timing table info.
  , _timingUIPartition :: Bool
  -- ^ Whether each module timing is partitioned into division
  , _timingUIHowParallel :: Bool
  -- ^ Whether showing color-coded parallel processes
  , _timingUIViewPortTopLeft :: (Double, Double)
  -- ^ Top-Left corner of timing UI viewport
  , _timingUIHandleMouseMove :: Bool
  }

makeClassy ''TimingUI

emptyTimingUI :: TimingUI
emptyTimingUI = TimingUI Nothing False False (0, 0) False

data ConsoleUI = ConsoleUI
  { _consoleFocus :: Maybe DriverId
  -- ^ focused console tab
  , _consoleInputEntry :: Text
  -- ^ console input entry
  }

makeClassy ''ConsoleUI

emptyConsoleUI :: ConsoleUI
emptyConsoleUI = ConsoleUI Nothing ""

data MainView = MainView
  { _mainTab :: Tab
  -- ^ current tab
  }

makeClassy ''MainView

emptyMainView :: MainView
emptyMainView =
  MainView
    { _mainTab = TabSession
    }

data UIModel = UIModel
  { _modelMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , _modelSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  , _modelSourceView :: SourceViewUI
  -- ^ UI state of source view UI
  , _modelTiming :: TimingUI
  -- ^ UI state of Timing UI
  , _modelConsole :: ConsoleUI
  }

makeClassy ''UIModel

emptyUIModel :: UIModel
emptyUIModel =
  UIModel
    { _modelMainModuleGraph = ModuleGraphUI Nothing Nothing
    , _modelSubModuleGraph = (UpTo30, ModuleGraphUI Nothing Nothing)
    , _modelSourceView = emptySourceViewUI
    , _modelTiming = emptyTimingUI
    , _modelConsole = emptyConsoleUI
    }

data UIView
  = BannerMode Double
  | MainMode MainView

makeClassy ''UIView

data UIState = UIState
  { _uiShouldUpdate :: Bool
  -- ^ should update?
  , _uiLastUpdated :: UTCTime
  -- ^ last updated time
  , _uiModel :: UIModel
  -- ^ main UI state
  , _uiView :: UIView
  -- ^ main view state
  , _uiAssets :: Assets
  -- ^ additional assets (such as png files)
  }

makeClassy ''UIState

emptyUIState :: Assets -> UTCTime -> UIState
emptyUIState assets now =
  UIState
    { _uiShouldUpdate = True
    , _uiLastUpdated = now
    , _uiModel = emptyUIModel
    , _uiView = BannerMode 0
    , _uiAssets = assets
    }
