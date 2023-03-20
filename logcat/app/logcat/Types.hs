{-# LANGUAGE TemplateHaskell #-}

module Types (
  -- * rectangle
  Rectangle (..),
  HasRectangle (..),

  -- * view model state
  ViewState (..),
  HasViewState (..),
  emptyViewState,

  -- * top-level logcat model state
  LogcatState (..),
  HasLogcatState (..),
  emptyLogcatState,

  -- * top-level view system state
  LogcatView (..),
  HasLogcatView (..),

  -- * Control Event
  CEvent (..),

  -- * heap size item
  HeapSizeItem (..),
) where

import Control.Lens (makeClassy)
import Data.Fixed (Nano)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq (empty)
import GHC.RTS.Events (Event (..))
import GI.Cairo.Render qualified as R
import GI.Pango qualified as P

data Rectangle = Rectangle
  { _rectX :: Double
  , _rectY :: Double
  , _rectW :: Double
  , _rectH :: Double
  }

makeClassy ''Rectangle

data ViewState = ViewState
  { _viewTimeOrigin :: Nano
  -- ^ start point of the timeline view
  , _viewLabelPositions :: Map String Rectangle
  -- ^ each event log type label positions
  , _viewHitted :: Maybe String
  }

makeClassy ''ViewState

emptyViewState :: ViewState
emptyViewState = ViewState 0 Map.empty Nothing

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event
  , -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event
  , _logcatEventHisto :: Map String Int
  , _logcatEventlogBytes :: Int
  , _logcatLastEventTime :: Nano
  , _logcatViewState :: ViewState
  }

makeClassy ''LogcatState

emptyLogcatState :: LogcatState
emptyLogcatState =
  LogcatState
    { _logcatEventStore = Seq.empty
    , _logcatEventQueue = Seq.empty
    , _logcatEventHisto = Map.empty
    , _logcatEventlogBytes = 0
    , _logcatLastEventTime = 0
    , _logcatViewState = emptyViewState
    }

-- | This holds all system-level view state such as cairo surfaces
data LogcatView = LogcatView
  { _logcatViewSurface :: R.Surface
  , _logcatViewPangoContext :: P.Context
  , _logcatViewFontDesc :: P.FontDescription
  , _logcatViewUpdater :: LogcatView -> IO ()
  }

makeClassy ''LogcatView

data CEvent
  = MotionNotify (Double, Double)
  | FlushEventQueue
  | RecordEvent Event
  | UpdateBytes Int
  deriving (Show)

data HeapSizeItem
  = HeapLive Int
  | BlocksSize Int
  | HeapSize Int
  deriving (Show)
