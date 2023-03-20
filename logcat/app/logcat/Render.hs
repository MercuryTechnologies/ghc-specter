{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (
  -- * draw all
  drawLogcatState,

  -- * flush double buffer
  flushDoubleBuffer,
) where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Lens (at, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Stat (drawStats)
import Render.Timeline (drawTimeline)
import Render.Util (
  clear,
  drawSeparator,
  drawText,
  fontSize,
  gray,
  red,
  setColor,
 )
import Types (
  HasLogcatState (..),
  HasViewState (..),
  LogcatState,
  LogcatView,
  Rectangle (..),
  ViewState,
 )

drawHistBar :: LogcatView -> ViewState -> (String, Int) -> R.Render ()
drawHistBar vw vs (ev, value) =
  for_ (vs ^. viewLabelPositions . at ev) $ \(Rectangle x y _ _) -> do
    if Just ev == vs ^. viewHitted
      then setColor red
      else setColor gray
    R.setLineWidth 1.0
    let w = fromIntegral value / 100.0
    drawText vw fontSize (x, y) (T.pack ev)
    R.rectangle (x + 100) (y + 2) w (fromIntegral fontSize)
    R.fill
    drawText vw fontSize (x + 104 + w, y) (T.pack (show value))

drawLogcatState :: LogcatView -> TVar LogcatState -> R.Render ()
drawLogcatState vw sref = do
  clear
  s <- liftIO $ atomically $ readTVar sref
  let evs = s ^. logcatEventStore
      hist = s ^. logcatEventHisto
      vs = s ^. logcatViewState
      nBytes = s ^. logcatEventlogBytes
  drawTimeline vw vs evs
  drawSeparator 150
  for_ (Map.toAscList hist) $ \(ev, value) ->
    drawHistBar vw vs (ev, value)
  drawStats vw nBytes

flushDoubleBuffer :: R.Surface -> R.Render ()
flushDoubleBuffer sfc = do
  R.setAntialias R.AntialiasNone
  R.setSourceSurface sfc 0 0
  R.setOperator R.OperatorSource
  R.paint
