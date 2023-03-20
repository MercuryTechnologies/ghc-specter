{-# OPTIONS_GHC -w #-}

module Render.Heap (
  drawHeapView,
) where

import Data.Fixed (Nano)
import Data.Foldable (for_)
import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Util (
  black,
  canvasHeight,
  canvasWidth,
  drawText,
  fontSize,
  gray,
  lightBlue,
  -- secToPixel,
  separatorPosY,
  setColor,
  transparentize,
  white,
  red,
 )
import Types (LogcatView, Rectangle (..))

scale = 1

-- | seconds to pixels in heap view frame
secToPixel :: Nano -> Nano -> Double
secToPixel origin sec =
  realToFrac (sec - origin) * scale

-- | pixels in heap view frame to seconds
pixelToSec :: Nano -> Double -> Nano
pixelToSec origin px =
  realToFrac (px / scale) + origin

drawGrid :: LogcatView -> Rectangle -> R.Render ()
drawGrid vw (Rectangle ulx uly w h) = do
  let origin = 0
      tmax = 1000
      ts = [0, 10 .. tmax]
      lblTs = [0, 60 .. tmax]

  R.setLineWidth 0.5
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  R.save
  R.translate ulx uly
  -- draw 10s lines
  setColor (transparentize gray)
  for_ ts $ \t -> do
    let x = secToPixel origin t
    R.moveTo x 0
    R.lineTo x h
    R.stroke
  -- draw 1m lines
  setColor (transparentize red)
  for_ lblTs $ \t -> do
    let x = secToPixel origin t
    R.moveTo x 0
    R.lineTo x h
    R.stroke
  -- labels
  setColor lightBlue
  for_ lblTs $ \t -> do
    let msg = T.pack (show (round (t / 60.0) :: Int) <> "m")
    drawText vw 6 (secToPixel origin t + 2, 0) msg
  R.restore

drawHeapView :: LogcatView -> R.Render ()
drawHeapView vw = do
  let ulx = canvasWidth - w - 10
      uly = separatorPosY + 10
      w = 800
      h = 80
  setColor black
  R.rectangle ulx uly w h
  R.fill
  setColor white
  R.rectangle ulx uly w h
  R.stroke
  let rect = Rectangle ulx uly w h
  drawGrid vw rect
