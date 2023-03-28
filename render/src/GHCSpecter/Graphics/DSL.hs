module GHCSpecter.Graphics.DSL (
  -- * data types
  Color (..),
  Primitive (..),
  TextPosition (..),
) where

import Data.Text (Text)

data Color = Black | White | Red | Blue | Green
  deriving (Show)

data TextPosition = UpperLeft | LowerLeft
  deriving (Show)

data Primitive
  = -- | (x, y) w h line_color background_color line_width
    Rectangle (Double, Double) Double Double (Maybe Color) (Maybe Color) (Maybe Double)
  | -- | start [bend_point] end line_color line_width
    Polyline (Double, Double) [(Double, Double)] (Double, Double) Color Double
  | -- | (x, y) text_pos font_size text
    DrawText (Double, Double) TextPosition Color Int Text
  deriving (Show)
