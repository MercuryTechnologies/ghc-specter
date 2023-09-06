{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Types
  ( -- * ViewPortInfo
    ViewPortInfo (..),

    -- * SessionUI
    SessionUI (..),
    emptySessionUI,

    -- * ModuleGraphUI
    ModuleGraphUI (..),
    emptyModuleGraphUI,

    -- * SourceViewUI
    SourceViewUI (..),
    emptySourceViewUI,

    -- * TimingUI
    TimingUI (..),
    emptyTimingUI,

    -- * BlockerUI
    BlockerUI (..),

    -- * ConsoleUI
    ConsoleUI (..),
    emptyConsoleUI,

    -- * UIModel
    UIModel (..),
    emptyUIModel,

    -- * UIState
    UIState (..),
    emptyUIState,
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (ViewPort (..))
import GHCSpecter.UI.Constants
  ( WidgetConfig,
    canvasDim,
    emptyWidgetConfig,
    modGraphHeight,
    modGraphWidth,
    sessionModStatusDim,
    timingHeight,
    timingWidth,
  )
import GHCSpecter.UI.Types.Event (DetailLevel (..), Tab (..))

data ViewPortInfo = ViewPortInfo
  { _vpViewPort :: ViewPort,
    _vpTempViewPort :: Maybe ViewPort
  }
  deriving (Show)

data SessionUI = SessionUI
  { _sessionUIModStatusViewPort :: ViewPortInfo,
    _sessionUIMainViewPort :: ViewPortInfo,
    _sessionUIProcessViewPort :: ViewPortInfo,
    _sessionUIRtsViewPort :: ViewPortInfo
  }

emptySessionUI :: SessionUI
emptySessionUI =
  SessionUI
    { _sessionUIModStatusViewPort = ViewPortInfo (ViewPort (0, 0) sessionModStatusDim) Nothing,
      _sessionUIMainViewPort = ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing,
      _sessionUIProcessViewPort = ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing,
      _sessionUIRtsViewPort = ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing
    }

data ModuleGraphUI = ModuleGraphUI
  { -- | module under mouse cursor in Module Graph
    _modGraphUIHover :: Maybe Text,
    -- | module clicked in Module Graph
    _modGraphUIClick :: Maybe Text,
    _modGraphViewPort :: ViewPortInfo
  }

emptyModuleGraphUI :: ModuleGraphUI
emptyModuleGraphUI =
  ModuleGraphUI
    { _modGraphUIHover = Nothing,
      _modGraphUIClick = Nothing,
      _modGraphViewPort = ViewPortInfo (ViewPort (0, 0) (modGraphWidth, modGraphHeight)) Nothing
    }

data SourceViewUI = SourceViewUI
  { -- | expanded module in SourceView
    _srcViewExpandedModule :: Maybe Text,
    -- | focused binding if exist
    _srcViewFocusedBinding :: Maybe Text,
    _srcViewSuppViewTab :: Maybe (Text, Int),
    _srcViewModuleTreeViewPort :: ViewPortInfo,
    _srcViewSourceViewPort :: ViewPortInfo,
    _srcViewSuppViewPort :: ViewPortInfo
  }

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
  { -- | When freezing timing flow, this holds the timing table info.
    _timingFrozenTable :: Maybe TimingTable,
    -- | Whether each module timing is partitioned into division
    _timingUIPartition :: Bool,
    -- | Whether showing color-coded parallel processes
    _timingUIHowParallel :: Bool,
    -- | timing UI viewport
    _timingUIViewPort :: ViewPortInfo,
    _timingUIHoveredModule :: Maybe Text
  }

emptyTimingUI :: TimingUI
emptyTimingUI =
  TimingUI
    { _timingFrozenTable = Nothing,
      _timingUIPartition = False,
      _timingUIHowParallel = False,
      _timingUIViewPort = ViewPortInfo (ViewPort (0, 0) (timingWidth, timingHeight)) Nothing,
      _timingUIHoveredModule = Nothing
    }

data BlockerUI = BlockerUI
  { _blockerUIViewPort :: ViewPortInfo
  }

emptyBlockerUI :: BlockerUI
emptyBlockerUI =
  BlockerUI
    { _blockerUIViewPort = ViewPortInfo (ViewPort (0, 0) (timingWidth, timingHeight)) Nothing
    }

data ConsoleUI = ConsoleUI
  { -- | focused console tab
    _consoleFocus :: Maybe DriverId,
    -- | console input entry
    _consoleInputEntry :: Text,
    -- | console viewport
    _consoleViewPort :: ViewPortInfo
  }

emptyConsoleUI :: ConsoleUI
emptyConsoleUI =
  ConsoleUI
    { _consoleFocus = Nothing,
      _consoleInputEntry = "",
      _consoleViewPort = ViewPortInfo (ViewPort (0, 0) canvasDim) Nothing
    }

data UIModel = UIModel
  { -- | current tab.
    _modelTab :: Tab,
    -- | programmatic destination of Tab (for :goto-source) in the next frame.
    _modelTabDestination :: Maybe Tab,
    -- | UI model of session
    _modelSession :: SessionUI,
    -- | UI model of main module graph
    _modelMainModuleGraph :: ModuleGraphUI,
    -- | UI model of sub module graph
    _modelSubModuleGraph :: (DetailLevel, ModuleGraphUI),
    -- | UI model of source view UI
    _modelSourceView :: SourceViewUI,
    -- | UI model of Timing UI
    _modelTiming :: TimingUI,
    -- | UI model of Blocker UI
    _modelBlocker :: BlockerUI,
    -- | UI model of console uI
    _modelConsole :: ConsoleUI,
    -- | widget config. to support dynamic configuration change
    _modelWidgetConfig :: WidgetConfig,
    -- | progress bar status.
    -- TODO: This will be handled more properly with typed transition.
    _modelTransientBanner :: Maybe Double
  }

emptyUIModel :: UIModel
emptyUIModel =
  UIModel
    { _modelTab = TabSession,
      _modelTabDestination = Nothing,
      _modelSession = emptySessionUI,
      _modelMainModuleGraph = emptyModuleGraphUI,
      _modelSubModuleGraph = (UpTo30, emptyModuleGraphUI),
      _modelSourceView = emptySourceViewUI,
      _modelTiming = emptyTimingUI,
      _modelBlocker = emptyBlockerUI,
      _modelConsole = emptyConsoleUI,
      _modelWidgetConfig = emptyWidgetConfig,
      _modelTransientBanner = Nothing
    }

{- NOTE: [UI state and model]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
   State is UI backend dependent and model is not, i.e. pertained to logical model of
   view. The candidates for subfields which is only in state are likely to be related
   to cache and rendering optimization. This line is naturally not very clear in the
   beginning, but will be clarified after more refactoring.

-}

data UIState = UIState
  { -- | should update?
    _uiShouldUpdate :: Bool,
    -- | last updated time
    _uiLastUpdated :: UTCTime,
    -- | main UI state
    _uiModel :: UIModel
  }

emptyUIState :: UTCTime -> UIState
emptyUIState now =
  UIState
    { _uiShouldUpdate = True,
      _uiLastUpdated = now,
      _uiModel = emptyUIModel
    }
