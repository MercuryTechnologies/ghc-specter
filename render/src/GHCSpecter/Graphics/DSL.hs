module GHCSpecter.Graphics.DSL (
  -- * enum types
  Color (..),
  TextFontFace (..),
  TextPosition (..),

  -- * event type
  HitEvent (..),

  -- * graphics primitives
  Primitive (..),
  ViewPort (..),
  Scene (..),

  -- * event primitives
  EventMap (..),
) where

import Data.Text (Text)

data Color
  = Black
  | White
  | Red
  | Blue
  | Green
  | Yellow
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

data TextFontFace = Sans | Mono
  deriving (Show)

data HitEvent e = HitEvent
  { hitEventHover :: Maybe e
  -- ^ event message when hovered
  , hitEventClick :: (Bool, Maybe e)
  -- ^ *current* activation status (toggle on/off), and event message when clicked
  }
  deriving (Show)

data Primitive e
  = -- | (x, y) w h line_color background_color line_width handle_hovering
    Rectangle (Double, Double) Double Double (Maybe Color) (Maybe Color) (Maybe Double) (Maybe (HitEvent e))
  | -- | start [bend_point] end line_color line_width
    Polyline (Double, Double) [(Double, Double)] (Double, Double) Color Double
  | -- | (x, y) text_pos font_size text
    DrawText (Double, Double) TextPosition TextFontFace Color Int Text
  deriving (Show)

data ViewPort = ViewPort
  { topLeft :: (Double, Double)
  , bottomRight :: (Double, Double)
  }
  deriving (Show)

-- scene has local view port matched with global canvas
data Scene e = Scene
  { sceneId :: Text
  , sceneGlobalViewPort :: ViewPort
  , sceneLocalViewPort :: ViewPort
  , sceneElements :: [Primitive e]
  }
  deriving (Show)

data EventMap e = EventMap
  { eventMapId :: Text
  , eventMapGlobalViewPort :: ViewPort
  , eventMapLocalViewPort :: ViewPort
  , eventMapElements :: [(HitEvent e, ViewPort)]
  }
  deriving (Show)
