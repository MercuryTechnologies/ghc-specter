{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.UI.Types (
  -- * ViewPortInfo
  ViewPortInfo (..),
  HasViewPortInfo (..),

  -- * SessionUI
  SessionUI (..),
  HasSessionUI (..),
  emptySessionUI,

  -- * ModuleGraphUI
  ModuleGraphUI (..),
  HasModuleGraphUI (..),
  emptyModuleGraphUI,

  -- * SourceViewUI
  SourceViewUI (..),
  HasSourceViewUI (..),
  emptySourceViewUI,

  -- * TimingUI
  TimingUI,
  HasTimingUI (..),
  emptyTimingUI,

  -- * ConsoleUI
  ConsoleUI,
  HasConsoleUI (..),
  emptyConsoleUI,

  -- * WidgetConfig
  WidgetConfig (..),
  HasWidgetConfig (..),
  emptyWidgetConfig,

  -- * UIModel
  UIModel (..),
  HasUIModel (..),
  emptyUIModel,

  -- * UIViewRaw
  UIViewRaw (..),
  HasUIViewRaw (..),

  -- * UIState
  UIState (..),
  HasUIState (..),
  emptyUIState,
) where

import Control.Lens (makeClassy)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Data.Assets (Assets)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (EventMap (..), ViewPort (..))
import GHCSpecter.UI.Constants (
  canvasDim,
  modGraphHeight,
  modGraphWidth,
  sessionModStatusDim,
  timingHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types.Event (DetailLevel (..), Tab (..))

data ViewPortInfo = ViewPortInfo
  { _vpViewPort :: ViewPort
  , _vpTempViewPort :: Maybe ViewPort
  }

makeClassy ''ViewPortInfo

data SessionUI = SessionUI
  { _sessionUIModStatusViewPort :: ViewPortInfo
  , _sessionUIMainViewPort :: ViewPortInfo
  }

makeClassy ''SessionUI

emptySessionUI :: SessionUI
emptySessionUI =
  SessionUI
    { _sessionUIModStatusViewPort = ViewPortInfo (ViewPort (0, 0) sessionModStatusDim) Nothing
    , _sessionUIMainViewPort = ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing
    }

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
  , _srcViewModuleTreeViewPort :: ViewPortInfo
  , _srcViewSourceViewPort :: ViewPortInfo
  , _srcViewSuppViewPort :: ViewPortInfo
  }

makeClassy ''SourceViewUI

emptySourceViewUI :: SourceViewUI
emptySourceViewUI =
  SourceViewUI
    Nothing
    Nothing
    Nothing
    (ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing)
    (ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing)
    (ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing)

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

-- | Each widget placing in the global canvas.
data WidgetConfig = WidgetConfig
  { _wcfgSession :: Map Text ViewPort
  , _wcfgModuleGraph :: Map Text ViewPort
  , _wcfgSourceView :: Map Text ViewPort
  , _wcfgTiming :: Map Text ViewPort
  }

makeClassy ''WidgetConfig

emptyWidgetConfig :: WidgetConfig
emptyWidgetConfig =
  WidgetConfig
    { _wcfgSession = Map.empty
    , _wcfgModuleGraph = Map.empty
    , _wcfgSourceView = Map.empty
    , _wcfgTiming = Map.empty
    }

data UIModel = UIModel
  { _modelTab :: Tab
  -- ^ current tab.
  , _modelSession :: SessionUI
  -- ^ UI model of session
  , _modelMainModuleGraph :: ModuleGraphUI
  -- ^ UI model of main module graph
  , _modelSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI model of sub module graph
  , _modelSourceView :: SourceViewUI
  -- ^ UI model of source view UI
  , _modelTiming :: TimingUI
  -- ^ UI model of Timing UI
  , _modelConsole :: ConsoleUI
  -- ^ UI model of console uI
  , _modelWidgetConfig :: WidgetConfig
  -- ^ widget config. to support dynamic configuration change
  }

makeClassy ''UIModel

emptyUIModel :: UIModel
emptyUIModel =
  UIModel
    { _modelTab = TabSession
    , _modelSession = emptySessionUI
    , _modelMainModuleGraph = emptyModuleGraphUI
    , _modelSubModuleGraph = (UpTo30, emptyModuleGraphUI)
    , _modelSourceView = emptySourceViewUI
    , _modelTiming = emptyTimingUI
    , _modelConsole = emptyConsoleUI
    , _modelWidgetConfig = emptyWidgetConfig
    }

data UIViewRaw = UIViewRaw
  { _uiTransientBanner :: Maybe Double
  -- ^ progress bar status.
  -- TODO: This will be handled more properly with typed transition.
  , _uiRawEventMap :: [EventMap Text]
  -- ^ event name -> bounding box map
  }

makeClassy ''UIViewRaw

{- NOTE: [UI state and model]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   State is UI backend dependent and model is not, i.e. pertained to logical model of
   view. The candidates for subfields which is only in state are likely to be related
   to cache and rendering optimization. This line is naturally not very clear in the
   beginning, but will be clarified after more refactoring.

-}

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
