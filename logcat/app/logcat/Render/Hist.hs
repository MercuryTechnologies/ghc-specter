module Render.Hist (
  drawHisto,
) where

import Control.Lens (at, (^.))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Util (
  drawText,
  fontSize,
  gray,
  red,
  setColor,
 )
import Types (
  LogcatView,
  Rectangle (..),
 )

drawHistBar ::
  LogcatView ->
  Map String Rectangle ->
  Maybe String ->
  (String, Int) ->
  R.Render ()
drawHistBar vw labelPos hitted (ev, value) =
  for_ (labelPos ^. at ev) $ \(Rectangle x y _ _) -> do
    let lblColor
          | Just ev == hitted = red
          | otherwise = gray
    setColor lblColor
    R.setLineWidth 1.0
    let w = fromIntegral value / 100.0
    drawText vw fontSize (x, y) (T.pack ev)
    R.rectangle (x + 100) (y + 2) w (fromIntegral fontSize)
    R.fill
    drawText vw fontSize (x + 104 + w, y) (T.pack (show value))

drawHisto ::
  LogcatView ->
  Map String Rectangle ->
  Maybe String ->
  Map String Int ->
  R.Render ()
drawHisto vw labelPos hitted histData =
  for_ (Map.toAscList histData) $ \(ev, value) ->
    drawHistBar vw labelPos hitted (ev, value)
