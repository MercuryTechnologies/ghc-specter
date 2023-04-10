module GHCSpecter.Graphics.DSL (
  -- * enum types
  Color (..),
  TextPosition (..),

  -- * graphics primitives
  Primitive (..),
  ViewPort (..),
  Scene (..),
) where

import Data.Text (Text)

data Color
  = Black
  | White
  | Red
  | Blue
  | Green
  | Gray
  | Orange
  | HoneyDew
  | Ivory
  | DimGray
  | LightGray
  | LightSlateGray
  | RoyalBlue
  | DeepSkyBlue
  | ColorRedLevel0
  | ColorRedLevel1
  | ColorRedLevel2
  | ColorRedLevel3
  | ColorRedLevel4
  | ColorRedLevel5
  deriving (Show)

data TextPosition = UpperLeft | LowerLeft
  deriving (Show)

data Primitive
  = -- | (x, y) w h line_color background_color line_width handle_hovering
    Rectangle (Double, Double) Double Double (Maybe Color) (Maybe Color) (Maybe Double) (Maybe Text)
  | -- | start [bend_point] end line_color line_width
    Polyline (Double, Double) [(Double, Double)] (Double, Double) Color Double
  | -- | (x, y) text_pos font_size text
    DrawText (Double, Double) TextPosition Color Int Text
  deriving (Show)

data ViewPort = ViewPort
  { topLeft :: (Double, Double)
  , bottomRight :: (Double, Double)
  }
  deriving (Show)

-- scene has local view port matched with global canvas
data Scene = Scene
  { sceneGlobalViewPort :: ViewPort
  , sceneLocalViewPort :: ViewPort
  , sceneElements :: [Primitive]
  }
