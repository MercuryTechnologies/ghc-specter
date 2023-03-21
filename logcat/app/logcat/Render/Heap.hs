{-# LANGUAGE LambdaCase #-}

module Render.Heap (
  heapViewWidth,
  secToPixel,
  pixelToSec,
  drawHeapView,
) where

import Control.Lens ((^.))
import Data.Fixed (Nano)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Util (
  black,
  canvasWidth,
  drawText,
  gray,
  green,
  lightBlue,
  red,
  separatorPosY,
  setColor,
  transparentize,
  white,
 )
import Types (
  HasViewState (..),
  HeapSizeItem (..),
  LogcatView,
  Rectangle (..),
  ViewState,
 )

scale :: Double
scale = 20

heapViewWidth :: Double
heapViewWidth = 800

-- | seconds to pixels in heap view frame
secToPixel :: Nano -> Nano -> Double
secToPixel origin sec =
  realToFrac (sec - origin) * scale

-- | pixels in heap view frame to seconds
pixelToSec :: Nano -> Double -> Nano
pixelToSec origin px =
  realToFrac (px / scale) + origin

drawGrid :: LogcatView -> ViewState -> Rectangle -> R.Render ()
drawGrid vw vs (Rectangle ulx uly _w h) = do
  let origin = vs ^. viewHeapOrigin
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

drawProfile :: ViewState -> Rectangle -> [(Nano, HeapSizeItem)] -> R.Render ()
drawProfile vs (Rectangle ulx uly _w h) profile = do
  let origin = vs ^. viewHeapOrigin
  let mheapSizes =
        NE.nonEmpty $
          mapMaybe (\case (t, HeapSize sz) -> Just (t, sz); _ -> Nothing) profile
      mblocksSizes =
        NE.nonEmpty $
          mapMaybe (\case (t, BlocksSize sz) -> Just (t, sz); _ -> Nothing) profile
      mheapLives =
        NE.nonEmpty $
          mapMaybe (\case (t, HeapLive sz) -> Just (t, sz); _ -> Nothing) profile
      maxSize =
        case mheapSizes of
          Nothing -> 10_000_000 -- 10 MB
          Just xs -> maximum (fmap snd xs)
      sz_scale = h / (fromIntegral maxSize * 1.2)
  R.setLineWidth 1
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  R.save
  R.translate ulx uly
  let showLine color ((t0, sz0) :| xs) = do
        setColor color
        R.moveTo (secToPixel origin t0) (h - fromIntegral sz0 * sz_scale)
        for_ xs $ \(t, sz) -> do
          let x = secToPixel origin t
          R.lineTo x (h - fromIntegral sz * sz_scale)
        R.stroke
  traverse_ (showLine red) mheapSizes
  traverse_ (showLine green) mblocksSizes
  traverse_ (showLine lightBlue) mheapLives
  R.restore

drawHeapView :: LogcatView -> ViewState -> [(Nano, HeapSizeItem)] -> R.Render ()
drawHeapView vw vs profile = do
  let ulx = canvasWidth - w - 10
      uly = separatorPosY + 10
      w = heapViewWidth
      h = 80
  setColor black
  R.rectangle ulx uly w h
  R.fill
  setColor white
  R.rectangle ulx uly w h
  R.stroke
  let rect = Rectangle ulx uly w h
  drawGrid vw vs rect
  drawProfile vs rect profile
