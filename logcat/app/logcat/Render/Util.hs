{-# LANGUAGE OverloadedLabels #-}

module Render.Util (
  -- * GUI parameters
  canvasWidth,
  canvasHeight,
  timelineMargin,
  xoffset,
  yoffset,
  fontSize,

  -- * conversion function
  secToPixel,
  pixelToSec,

  -- * colors
  black,
  white,
  blue,
  lightBlue,
  red,
  gray,

  -- * color util
  transparentize,
  setColor,

  -- * draw util
  clear,
  drawText,
  drawSeparator,
) where

import Control.Lens ((^.))
import Data.Fixed (Nano)
import Data.Int (Int32)
import Data.Text (Text)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC
import Types (
  HasLogcatView (..),
  LogcatView,
 )

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

fontSize :: Int32
fontSize = 8

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

lightBlue :: (Double, Double, Double, Double)
lightBlue = (0.678, 0.847, 0.902, 1)

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

drawSeparator :: Double -> R.Render ()
drawSeparator y = do
  setColor gray
  R.setLineWidth 1
  R.moveTo 0 y
  R.lineTo canvasWidth y
  R.stroke

clear :: R.Render ()
clear = do
  setColor black
  R.rectangle 0 0 canvasWidth canvasHeight
  R.fill
