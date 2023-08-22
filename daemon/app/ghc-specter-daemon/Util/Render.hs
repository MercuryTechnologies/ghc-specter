{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util.Render
  ( ImRenderState (..),
    ImRender (..),
    runImRender,
    renderPrimitive,
    rgb2Color,
    hexRGB2Color,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteString (useAsCString)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import FFICXX.Runtime.Cast (FPtr (cast_fptr_to_obj))
import Foreign.C.Types (CFloat, CUInt)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Graphics.DSL
  ( Color (..),
    DrawText (..),
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Shape (..),
    TextFontFace (..),
    TextPosition (LowerLeft, UpperLeft),
  )
import ImGui
import STD.Deletable (delete)
import Util.Orphans ()

data ImRenderState = ImRenderState
  { currDrawList :: ImDrawList,
    currOrigin :: (CFloat, CFloat),
    currFontSans :: ImFont,
    currFontMono :: ImFont
  }

newtype ImRender a = ImRender
  { unImRender :: ReaderT ImRenderState IO a
  }
  deriving (Functor, Applicative, Monad)

runImRender :: ImRenderState -> ImRender a -> IO a
runImRender s action = runReaderT (unImRender action) s

--
--
--

renderPrimitive ::
  Primitive e ->
  ImRender ()
renderPrimitive (Primitive (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) _ _mhitEvent) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        x' = ox + realToFrac x
        y' = oy + realToFrac y
        w' = realToFrac w
        h' = realToFrac h
    v1 <- newImVec2 x' y'
    v2 <- newImVec2 (x' + w') (y' + h')
    for_ mbkg $ \bkg -> do
      col <- getNamedColor bkg
      imDrawList_AddRectFilled
        s.currDrawList
        v1
        v2
        col
        0.0
        0 -- no flag
    for_ ((,) <$> mline <*> mlwidth) $ \(line, lwidth) -> do
      col <- getNamedColor line
      imDrawList_AddRect
        s.currDrawList
        v1
        v2
        col
        0.0
        0 -- no flag
        (realToFrac lwidth)
    delete v1
    delete v2
renderPrimitive (Primitive (SPolyline (Polyline xy0 xys xy1 color swidth)) _ _) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        (x0, y0) = xy0
        (x1, y1) = xy1
        nPoints = length xys + 2
    -- TODO: make a utility function for this tedious and error-prone process
    allocaArray nPoints $ \(pp :: Ptr ImVec2) -> do
      p0 <- newImVec2 (realToFrac x0 + ox) (realToFrac y0 + oy)
      pokeElemOff pp 0 p0
      delete p0
      p1 <- newImVec2 (realToFrac x1 + ox) (realToFrac y1 + oy)
      pokeElemOff pp (nPoints - 1) p1
      delete p1
      for_ (zip [1 ..] xys) $ \(i, (x, y)) -> do
        p <- newImVec2 (realToFrac x + ox) (realToFrac y + oy)
        pokeElemOff pp i p
        delete p
      let p :: ImVec2 = cast_fptr_to_obj (castPtr pp)
      col <- getNamedColor color
      imDrawList_AddPolyline
        s.currDrawList
        p
        (fromIntegral nPoints)
        col
        0
        (realToFrac swidth)
renderPrimitive (Primitive (SDrawText (DrawText (x, y) pos font color fontSize msg)) _ _) = ImRender $ do
  s <- ask
  liftIO $ do
    case font of
      Sans -> pushFont (s.currFontSans)
      Mono -> pushFont (s.currFontMono)
    let (ox, oy) = s.currOrigin
        offsetY = case pos of
          UpperLeft -> 0
          LowerLeft -> -fontSize
        x' = realToFrac x + ox
        y' = realToFrac y + oy + fromIntegral offsetY
    v' <- newImVec2 x' y'
    col <- getNamedColor color
    useAsCString (encodeUtf8 msg) $ \cstr ->
      imDrawList_AddText
        s.currDrawList
        v'
        col
        cstr
    delete v'
    popFont

--
-- Color
--

rgb2Color :: (Int, Int, Int) -> IO CUInt
rgb2Color (r, g, b) = do
  colf <- newImVec4 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0) 1.0
  col <- colorConvertFloat4ToU32 colf
  delete colf
  pure col

hexRGB2Color :: Int -> IO CUInt
hexRGB2Color h = do
  let (r, gb) = h `divMod` (256 * 256)
      (g, b) = gb `divMod` 256
  rgb2Color (r, g, b)

getNamedColor :: Color -> IO CUInt
getNamedColor Black = hexRGB2Color 0x000000
getNamedColor White = hexRGB2Color 0xffffff
getNamedColor Red = hexRGB2Color 0xff0000
getNamedColor Blue = hexRGB2Color 0x0000ff
getNamedColor Green = hexRGB2Color 0x008000
getNamedColor Yellow = hexRGB2Color 0xffff00
getNamedColor Gray = hexRGB2Color 0x808080
getNamedColor Orange = hexRGB2Color 0xffa500
getNamedColor HoneyDew = hexRGB2Color 0xf0fff0
getNamedColor Ivory = hexRGB2Color 0xfffff0
getNamedColor DimGray = hexRGB2Color 0x696969
getNamedColor LightGray = hexRGB2Color 0xd3d3d3
getNamedColor LightSlateGray = hexRGB2Color 0x778899
getNamedColor RoyalBlue = hexRGB2Color 0x4169e1
getNamedColor DeepSkyBlue = hexRGB2Color 0x00bfff
getNamedColor ColorRedLevel0 = hexRGB2Color 0xffffff
getNamedColor ColorRedLevel1 = hexRGB2Color 0xfdedec
getNamedColor ColorRedLevel2 = hexRGB2Color 0xfadbd8
getNamedColor ColorRedLevel3 = hexRGB2Color 0xf5b7b1
getNamedColor ColorRedLevel4 = hexRGB2Color 0xf1948a
getNamedColor ColorRedLevel5 = hexRGB2Color 0xec7063
