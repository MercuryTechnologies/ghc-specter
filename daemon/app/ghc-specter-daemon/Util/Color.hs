module Util.Color
  ( rgb2Color,
    hexRGB2Color,
    getNamedColor,
  )
where

import Foreign.C.Types (CUInt)
import GHCSpecter.Graphics.DSL (Color (..))
import ImGui
import STD.Deletable (delete)
import Util.Orphans ()

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
