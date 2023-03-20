module Render.Timeline (
  drawTimeline,
) where

import Control.Lens ((^.))
import Data.Fixed (Fixed (MkFixed), Nano)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text qualified as T
import GHC.RTS.Events (Event (..))
import GI.Cairo.Render qualified as R
import Render.Util (
  canvasWidth,
  drawText,
  fontSize,
  gray,
  lightBlue,
  pixelToSec,
  red,
  secToPixel,
  setColor,
  transparentize,
  white,
 )
import Types (
  HasViewState (..),
  LogcatView,
  ViewState,
 )
import Util.Event (eventInfoEnumMap, eventInfoToString)

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
  setColor lightBlue
  for_ lblTs $ \t -> do
    let msg = T.pack (show (floor t :: Int) <> " s")
    drawText vw fontSize (secToPixel origin t + 4, 0) msg

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
