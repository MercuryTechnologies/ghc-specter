{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (
  -- * draw all
  drawLogcatState,

  -- * flush double buffer
  flushDoubleBuffer,
) where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Fixed (..))
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import GHC.RTS.Events qualified as RTS
import GI.Cairo.Render qualified as R
import Render.Heap (drawHeapView)
import Render.Hist (drawHisto)
import Render.Stat (drawStats)
import Render.Timeline (drawTimeline)
import Render.Util (
  clear,
  drawSeparator,
  separatorPosY,
 )
import Types (
  HasLogcatState (..),
  HasViewState (..),
  HeapSizeItem (..),
  LogcatState,
  LogcatView,
 )

drawLogcatState :: LogcatView -> TVar LogcatState -> R.Render ()
drawLogcatState vw sref = do
  clear
  s <- liftIO $ atomically $ readTVar sref
  let evs = s ^. logcatEventStore
      hist = s ^. logcatEventHisto
      vs = s ^. logcatViewState
      labelPos = vs ^. viewLabelPositions
      hitted = vs ^. viewHitted
      nBytes = s ^. logcatEventlogBytes

      filterHeapSize ev =
        case RTS.evSpec ev of
          RTS.HeapLive {RTS.liveBytes} -> Just $ HeapLive (fromIntegral liveBytes)
          RTS.BlocksSize {RTS.blocksSize} -> Just $ BlocksSize (fromIntegral blocksSize)
          RTS.HeapSize {RTS.sizeBytes} -> Just $ HeapSize (fromIntegral sizeBytes)
          _ -> Nothing
      evs' = mapMaybe (\ev -> (,) (MkFixed (fromIntegral (RTS.evTime ev))) <$> filterHeapSize ev) (toList evs)
  drawTimeline vw vs evs
  drawSeparator separatorPosY
  drawHeapView vw evs'
  drawHisto vw labelPos hitted hist
  drawStats vw nBytes

flushDoubleBuffer :: R.Surface -> R.Render ()
flushDoubleBuffer sfc = do
  R.setAntialias R.AntialiasNone
  R.setSourceSurface sfc 0 0
  R.setOperator R.OperatorSource
  R.paint
