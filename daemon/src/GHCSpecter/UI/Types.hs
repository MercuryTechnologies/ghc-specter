{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.UI.Types
  ( -- * UI state
    ModuleGraphUI (..),
    HasModuleGraphUI (..),
    emptyModuleGraphUI,
    SourceViewUI (..),
    HasSourceViewUI (..),
    emptySourceViewUI,
    TimingUI,
    HasTimingUI (..),
    emptyTimingUI,
    MainView (..),
    HasMainView (..),
    emptyMainView,
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

data MainView = MainView
  { _mainTab :: Tab
  -- ^ current tab
  , _mainMainModuleGraph :: ModuleGraphUI
  -- ^ UI state of main module graph
  , _mainSubModuleGraph :: (DetailLevel, ModuleGraphUI)
  -- ^ UI state of sub module graph
  , _mainSourceView :: SourceViewUI
  -- ^ UI state of source view UI
  , _mainTiming :: TimingUI
  -- ^ UI state of Timing UI
  , _mainMousePosition :: (Double, Double)
  -- ^ mouse position
  }

makeClassy ''MainView

emptyMainView :: MainView
emptyMainView =
  MainView
    { _mainTab = TabSession
    , _mainMainModuleGraph = ModuleGraphUI Nothing Nothing
    , _mainSubModuleGraph = (UpTo30, ModuleGraphUI Nothing Nothing)
    , _mainSourceView = emptySourceViewUI
    , _mainTiming = emptyTimingUI
    , _mainMousePosition = (0, 0)
    }

data UIView = BannerMode | MainMode MainView

makeClassy ''UIView

-- makePrisms ''UIView

data UIState = UIState
  { _uiShouldUpdate :: Bool
  -- ^ should update?
  , _uiLastUpdated :: UTCTime
  -- ^ last updated time
  , _uiView :: UIView
  }

makeClassy ''UIState

emptyUIState :: UTCTime -> UIState
emptyUIState now =
  UIState
    { _uiShouldUpdate = True
    , _uiLastUpdated = now
    , _uiView = BannerMode
    }
