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
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import GHC.RTS.Events (Event (..))
import GI.Cairo.Render qualified as R
import Types (
  HasLogcatState (..),
  HasViewState (..),
  LogcatState,
  Rectangle (..),
  ViewState,
 )
import Util.Event (eventInfoEnumMap, eventInfoToString)

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

drawEventMark :: ViewState -> Event -> R.Render ()
drawEventMark vs ev = do
  let origin = vs ^. viewTimeOrigin
      sec = MkFixed (fromIntegral (evTime ev)) :: Nano
      x = secToPixel origin sec
      evname = eventInfoToString (evSpec ev)
      tag = fromMaybe 0 (L.lookup evname eventInfoEnumMap)
      y = fromIntegral tag * 3.0
  if Just evname == vs ^. viewHitted
    then R.setSourceRGBA 1 0 0 1.0
    else R.setSourceRGBA 0.16 0.18 0.19 1.0
  R.moveTo x y
  R.lineTo x (y + 2)
  R.stroke

drawHighlighter :: ViewState -> R.Render ()
drawHighlighter vs = do
  let mhitted = vs ^. viewHitted
  for_ mhitted $ \hitted -> do
    let tag = fromMaybe 0 (L.lookup hitted eventInfoEnumMap)
        y = fromIntegral tag * 3.0
    R.setSourceRGBA 0 0 0 0.1
    R.setLineWidth 2.0
    R.moveTo 0 y
    R.lineTo canvasWidth y
    R.stroke

drawTimeGrid :: ViewState -> R.Render ()
drawTimeGrid vs = do
  let origin = vs ^. viewTimeOrigin
      tmax = pixelToSec origin canvasWidth
      ts = [0, 1 .. tmax]
      lblTs = [0, 10 .. tmax]
  R.setSourceRGBA 0 0 1 0.5
  R.setLineWidth 0.1
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ ts $ \t -> do
    let x = secToPixel origin t
    R.moveTo x 0
    R.lineTo x 150
    R.stroke
  R.setSourceRGBA 0 0 1 0.8
  R.setFontSize 8
  for_ lblTs $ \t -> do
    R.moveTo (secToPixel origin t) 10
    R.textPath (show (floor t :: Int) <> " s")
    R.fill

drawTimeline :: ViewState -> Seq Event -> R.Render ()
drawTimeline vs evs = do
  -- time grid
  drawTimeGrid vs
  -- highlight hitted event row
  drawHighlighter vs
  -- draw actual events
  R.setLineWidth 1.0
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ evs $ \ev ->
    drawEventMark vs ev

drawHistBar :: ViewState -> (String, Int) -> R.Render ()
drawHistBar vs (ev, value) =
  for_ (vs ^. viewLabelPositions . at ev) $ \(Rectangle x y _ _) -> do
    if Just ev == vs ^. viewHitted
      then R.setSourceRGBA 1 0 0 1.0
      else R.setSourceRGBA 0.16 0.18 0.19 1.0
    R.setLineWidth 1.0
    let w = fromIntegral value / 100.0
    R.moveTo x (y + 10.0)
    R.setFontSize 8.0
    R.textPath ev
    R.fill
    R.rectangle (x + 100) (y + 2) w 8
    R.fill
    R.moveTo (x + 104 + w) (y + 10.0)
    R.textPath (show value)
    R.fill

drawLogcatState :: TVar LogcatState -> R.Render ()
drawLogcatState sref = do
  R.setSourceRGB 1 1 1
  R.rectangle 0 0 canvasWidth canvasHeight
  R.fill
  s <- liftIO $ atomically $ readTVar sref
  let evs = s ^. logcatEventStore
      hist = s ^. logcatEventHisto
      vs = s ^. logcatViewState
  drawTimeline vs evs
  R.setSourceRGBA 0 0 1 0.8
  R.setLineWidth 1
  R.moveTo 0 150
  R.lineTo canvasWidth 150
  R.stroke
  for_ (Map.toAscList hist) $ \(ev, value) ->
    drawHistBar vs (ev, value)

flushDoubleBuffer :: R.Surface -> R.Render ()
flushDoubleBuffer sfc = do
  R.setAntialias R.AntialiasNone
  R.setSourceSurface sfc 0 0
  R.setOperator R.OperatorSource
  R.paint
