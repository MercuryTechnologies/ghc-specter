module GHCSpecter.Graphics.DSL (
  -- * data types
  Color (..),
  Primitive (..),
) where

import Data.Text (Text)

data Color = Black | White | Red | Blue | Green | Transparent
  deriving (Show)

data TextPosition = UpperLeft | LowerLeft
  deriving (Show)

data Primitive
  -- | (x, y) w h line_color background_color line_width
  = Rectangle (Double, Double) Double Double Color Color Double
  -- | start [bend_point] end line_color line_width
  | Polyline (Double, Double) [(Double, Double)] (Double, Double) Color Double
  -- | (x, y) text_pos font_size text
  | DrawText (Double, Double) TextPosition Int Text
  deriving (Show)
