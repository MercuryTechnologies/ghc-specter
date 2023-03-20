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
import GI.Cairo.Render qualified as R
import Render.Hist (drawHisto)
import Render.Stat (drawStats)
import Render.Timeline (drawTimeline)
import Render.Util (
  clear,
  drawSeparator,
 )
import Types (
  HasLogcatState (..),
  HasViewState (..),
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
  drawTimeline vw vs evs
  drawSeparator 150
  drawHisto vw labelPos hitted hist
  drawStats vw nBytes

flushDoubleBuffer :: R.Surface -> R.Render ()
flushDoubleBuffer sfc = do
  R.setAntialias R.AntialiasNone
  R.setSourceSurface sfc 0 0
  R.setOperator R.OperatorSource
  R.paint
