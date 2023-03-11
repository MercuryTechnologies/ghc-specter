{-# LANGUAGE TemplateHaskell #-}

module Types (
  -- * rectangle
  Rectangle (..),
  HasRectangle (..),

  -- * view state
  ViewState (..),
  HasViewState (..),
  emptyViewState,

  -- * top-level logcat state
  LogcatState (..),
  HasLogcatState (..),
  emptyLogcatState,
) where

import Control.Lens (makeClassy)
import Data.Fixed (Nano)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq (empty)
import GHC.RTS.Events (Event (..))

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
  }

makeClassy ''ViewState

emptyViewState :: ViewState
emptyViewState = ViewState 0 Map.empty

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event
  , -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event
  , _logcatEventHisto :: Map String Int
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
    , _logcatLastEventTime = 0
    , _logcatViewState = emptyViewState
    }
