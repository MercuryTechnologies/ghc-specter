{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.UI.Types (
  ViewPort (..),
  ModuleGraphUI (..),
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
  UIModel (..),
  HasUIModel (..),
  emptyUIModel,
  UIViewRaw (..),
  HasUIViewRaw (..),
  ViewPortInfo (..),
  HasViewPortInfo (..),
  UIState (..),
  HasUIState (..),
  emptyUIState,
) where

import Control.Lens (makeClassy)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Data.Assets (Assets)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.UI.Constants (
  modGraphHeight,
  modGraphWidth,
  timingHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types.Event (DetailLevel (..), Tab (..))

data ViewPort = ViewPort
  { topLeft :: (Double, Double)
  , bottomRight :: (Double, Double)
  }
  deriving (Show)

data ViewPortInfo = ViewPortInfo
  { _vpViewPort :: ViewPort
  , _vpTempViewPort :: Maybe ViewPort
  }

makeClassy ''ViewPortInfo

data ModuleGraphUI = ModuleGraphUI
  { _modGraphUIHover :: Maybe Text
  -- ^ module under mouse cursor in Module Graph
  , _modGraphUIClick :: Maybe Text
  -- ^ module clicked in Module Graph
  , _modGraphViewPort :: ViewPortInfo
  }

makeClassy ''ModuleGraphUI

emptyModuleGraphUI :: ModuleGraphUI
emptyModuleGraphUI =
  ModuleGraphUI
    { _modGraphUIHover = Nothing
    , _modGraphUIClick = Nothing
    , _modGraphViewPort = ViewPortInfo (ViewPort (0, 0) (modGraphWidth, modGraphHeight)) Nothing
    }

data SourceViewUI = SourceViewUI
  { _srcViewExpandedModule :: Maybe Text
  -- ^ expanded module in SourceView
  , _srcViewFocusedBinding :: Maybe Text
  -- ^ focused binding if exist
  , _srcViewSuppViewTab :: Maybe (Text, Int)
  }

makeClassy ''SourceViewUI

emptySourceViewUI :: SourceViewUI
emptySourceViewUI = SourceViewUI Nothing Nothing Nothing

data TimingUI = TimingUI
  { _timingFrozenTable :: Maybe TimingTable
  -- ^ When freezing timing flow, this holds the timing table info.
  , _timingUIPartition :: Bool
  -- ^ Whether each module timing is partitioned into division
  , _timingUIHowParallel :: Bool
  -- ^ Whether showing color-coded parallel processes
  , _timingUIViewPort :: ViewPortInfo
  -- ^ timing UI viewport
  , _timingUIHandleMouseMove :: Bool
  , _timingUIHoveredModule :: Maybe Text
  , _timingUIBlockerGraph :: Bool
  }

makeClassy ''TimingUI

emptyTimingUI :: TimingUI
emptyTimingUI =
  TimingUI
    { _timingFrozenTable = Nothing
    , _timingUIPartition = False
    , _timingUIHowParallel = False
    , _timingUIViewPort = ViewPortInfo (ViewPort (0, 0) (timingWidth, timingHeight)) Nothing
    , _timingUIHandleMouseMove = False
    , _timingUIHoveredModule = Nothing
    , _timingUIBlockerGraph = False
    }

data ConsoleUI = ConsoleUI
  { _consoleFocus :: Maybe DriverId
  -- ^ focused console tab
  , _consoleInputEntry :: Text
  -- ^ console input entry
  }

makeClassy ''ConsoleUI

emptyConsoleUI :: ConsoleUI
emptyConsoleUI = ConsoleUI Nothing ""

data UIModel = UIModel
  { _modelTab :: Tab
  -- ^ current tab.
  , _modelMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , _modelSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  , _modelSourceView :: SourceViewUI
  -- ^ UI state of source view UI
  , _modelTiming :: TimingUI
  -- ^ UI state of Timing UI
  , _modelConsole :: ConsoleUI
  -- ^ UI state of console uI
  }

makeClassy ''UIModel

emptyUIModel :: UIModel
emptyUIModel =
  UIModel
    { _modelTab = TabSession
    , _modelMainModuleGraph = emptyModuleGraphUI
    , _modelSubModuleGraph = (UpTo30, emptyModuleGraphUI)
    , _modelSourceView = emptySourceViewUI
    , _modelTiming = emptyTimingUI
    , _modelConsole = emptyConsoleUI
    }

data UIViewRaw = UIViewRaw
  { _uiTransientBanner :: Maybe Double
  -- ^ progress bar status.
  -- TODO: This will be handled more properly with typed transition.
  , _uiRawEventMap :: [(Text, ((Double, Double), (Double, Double)))]
  -- ^ event name -> bounding box map
  }

makeClassy ''UIViewRaw

data UIState = UIState
  { _uiShouldUpdate :: Bool
  -- ^ should update?
  , _uiLastUpdated :: UTCTime
  -- ^ last updated time
  , _uiModel :: UIModel
  -- ^ main UI state
  , _uiViewRaw :: UIViewRaw
  -- ^ view state in the raw
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
    , _uiViewRaw = UIViewRaw Nothing []
    , _uiAssets = assets
    }
