{-# LANGUAGE LambdaCase #-}

module Render.Heap (
  drawHeapView,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Nano)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Util (
  black,
  canvasHeight,
  canvasWidth,
  drawText,
  fontSize,
  gray,
  green,
  lightBlue,
  separatorPosY,
  setColor,
  transparentize,
  white,
  red,
 )
import Types (
  HeapSizeItem (..),
  LogcatView,
  Rectangle (..),
 )

scale :: Double
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

drawProfile :: LogcatView -> Rectangle -> [(Nano, HeapSizeItem)] -> R.Render ()
drawProfile vw (Rectangle ulx uly w h) profile = do
  let origin = 0
  let heapSizes =
        mapMaybe (\case (t, HeapSize sz) -> Just (t, sz); _ -> Nothing) profile
      blocksSizes =
        mapMaybe (\case (t, BlocksSize sz) -> Just (t, sz); _ -> Nothing) profile
      heapLives =
        mapMaybe (\case (t, HeapLive sz) -> Just (t, sz); _ -> Nothing) profile
      maxSize =
        case heapSizes of
          [] -> 10_000_000 -- 10 MB
          _ -> maximum (fmap snd heapSizes)
      sz_scale = h / (fromIntegral maxSize * 1.2)
  R.setLineWidth 1
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  R.save
  R.translate ulx uly
  let showLine color dat = do
        setColor color
        R.moveTo 0 h
        for_ dat $ \(t, sz) -> do
          let x = secToPixel origin t
          R.lineTo (realToFrac t) (h - fromIntegral sz * sz_scale)
        R.stroke
  showLine red heapSizes
  showLine green blocksSizes
  showLine lightBlue heapLives
  R.restore

drawHeapView :: LogcatView -> [(Nano, HeapSizeItem)] -> R.Render ()
drawHeapView vw profile = do
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
  drawProfile vw rect profile
