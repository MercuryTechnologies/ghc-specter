{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (
  -- * GUI parameters
  canvasWidth,
  canvasHeight,
  timelineMargin,
  xoffset,
  yoffset,

  -- * conversion function
  secToPixel,
  pixelToSec,

  -- * draw functions
  drawEventMark,
  drawTimeGrid,
  drawTimeline,
  drawHistBar,
  drawLogcatState,

  -- * flush double buffer
  flushDoubleBuffer,
) where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Lens (at, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Fixed (MkFixed), Nano)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.RTS.Events (Event (..))
import GI.Cairo.Render qualified as R
import Types (
  HasLogcatState (..),
  HasLogcatView (..),
  HasViewState (..),
  LogcatState,
  LogcatView,
  Rectangle (..),
  ViewState,
 )
import Util.Event (eventInfoEnumMap, eventInfoToString)
--
import GI.Cairo.Render.Connector qualified as RC
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

canvasWidth :: Double
canvasWidth = 1440

canvasHeight :: Double
canvasHeight = 768

timelineMargin :: Double
timelineMargin = 300

timelineScale :: Double
timelineScale = 50

xoffset :: Double
xoffset = 10

yoffset :: Double
yoffset = 100

secToPixel :: Nano -> Nano -> Double
secToPixel origin sec =
  realToFrac (sec - origin) * timelineScale + 10.0

pixelToSec :: Nano -> Double -> Nano
pixelToSec origin px =
  realToFrac ((px - 10.0) / timelineScale) + origin

black :: (Double, Double, Double, Double)
black = (0, 0, 0, 1)

white :: (Double, Double, Double, Double)
white = (1, 1, 1, 1)

blue :: (Double, Double, Double, Double)
blue = (0, 0, 1, 1)

red :: (Double, Double, Double, Double)
red = (1, 0, 0, 1)

gray :: (Double, Double, Double, Double)
gray = (0.5, 0.5, 0.5, 1)

transparentize :: (Double, Double, Double, Double) -> (Double, Double, Double, Double)
transparentize (r, g, b, _) = (r, g, b, 0.5)

setColor :: (Double, Double, Double, Double) -> R.Render ()
setColor (r, g, b, a) = R.setSourceRGBA r g b a

drawText :: LogcatView -> Int32 -> (Double, Double) -> Text -> R.Render ()
drawText vw sz (x, y) msg = do
  let pangoCtxt = vw ^. logcatViewPangoContext
      desc = vw ^. logcatViewFontDesc
  layout :: P.Layout <- P.layoutNew pangoCtxt
  #setSize desc (sz * P.SCALE)
  #setFontDescription layout (Just desc)
  #setText layout msg (-1)
  R.moveTo x y
  ctxt <- RC.getContext
  PC.showLayout ctxt layout

drawEventMark :: ViewState -> Event -> R.Render ()
drawEventMark vs ev = do
  let origin = vs ^. viewTimeOrigin
      sec = MkFixed (fromIntegral (evTime ev)) :: Nano
      x = secToPixel origin sec
      evname = eventInfoToString (evSpec ev)
      tag = fromMaybe 0 (L.lookup evname eventInfoEnumMap)
      y = fromIntegral tag * 3.0
  if Just evname == vs ^. viewHitted
    then setColor red
    else setColor white
  R.moveTo x y
  R.lineTo x (y + 2)
  R.stroke

drawHighlighter :: ViewState -> R.Render ()
drawHighlighter vs = do
  let mhitted = vs ^. viewHitted
  for_ mhitted $ \hitted -> do
    let tag = fromMaybe 0 (L.lookup hitted eventInfoEnumMap)
        y = fromIntegral tag * 3.0
    setColor (transparentize white)
    R.setLineWidth 2.0
    R.moveTo 0 y
    R.lineTo canvasWidth y
    R.stroke

drawTimeGrid :: LogcatView -> ViewState -> R.Render ()
drawTimeGrid vw vs = do
  let origin = vs ^. viewTimeOrigin
      tmax = pixelToSec origin canvasWidth
      ts = [0, 1 .. tmax]
      lblTs = [0, 10 .. tmax]
  setColor (transparentize gray)
  R.setLineWidth 0.5
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ ts $ \t -> do
    let x = secToPixel origin t
    R.moveTo x 0
    R.lineTo x 150
    R.stroke
  setColor blue
  for_ lblTs $ \t -> do
    let msg = T.pack (show (floor t :: Int) <> " s")
    drawText vw 6 (secToPixel origin t + 4, 0) msg

drawTimeline :: LogcatView -> ViewState -> Seq Event -> R.Render ()
drawTimeline vw vs evs = do
  -- time grid
  drawTimeGrid vw vs
  -- highlight hitted event row
  drawHighlighter vs
  -- draw actual events
  R.setLineWidth 1.0
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ evs $ \ev ->
    drawEventMark vs ev

drawHistBar :: LogcatView -> ViewState -> (String, Int) -> R.Render ()
drawHistBar vw vs (ev, value) =
  for_ (vs ^. viewLabelPositions . at ev) $ \(Rectangle x y _ _) -> do
    if Just ev == vs ^. viewHitted
      then setColor red
      else setColor gray
    R.setLineWidth 1.0
    let w = fromIntegral value / 100.0
    drawText vw 6 (x, y) (T.pack ev)
    R.rectangle (x + 100) (y + 2) w 6
    R.fill
    drawText vw 6 (x + 104 + w, y) (T.pack (show value))

drawSeparator :: Double -> R.Render ()
drawSeparator y = do
  setColor gray
  R.setLineWidth 1
  R.moveTo 0 150
  R.lineTo canvasWidth y
  R.stroke

clear :: R.Render ()
clear = do
  setColor black
  R.rectangle 0 0 canvasWidth canvasHeight
  R.fill

drawStats :: LogcatView -> Int -> R.Render ()
drawStats vw nBytes = do
  let ulx = canvasWidth - w - 10
      uly = canvasHeight - h - 10
      w = 150
      h = 80
  setColor black
  R.rectangle ulx uly w h
  R.fill
  setColor white
  R.rectangle ulx uly w h
  R.stroke

  let msg = T.pack $ "Received bytes: " ++ show nBytes
  setColor white
  drawText vw 6 (ulx + 5, uly + 5) msg

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
