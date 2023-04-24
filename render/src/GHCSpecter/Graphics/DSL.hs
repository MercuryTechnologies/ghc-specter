module GHCSpecter.Graphics.DSL (
  -- * enum types
  Color (..),
  TextFontFace (..),
  TextPosition (..),

  -- * view port
  ViewPort (..),
  viewPortWidth,
  viewPortHeight,

  -- * event type
  HitEvent (..),

  -- * graphics primitives
  Rectangle (..),
  Polyline (..),
  DrawText (..),
  Shape (..),
  Primitive (..),
  Scene (..),

  -- * smart constructors
  rectangle,
  polyline,
  drawText,

  -- * event primitives
  EventMap (..),
) where

import Data.Bifunctor (bimap)
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

--
-- ViewPort
--

data ViewPort = ViewPort
  { topLeft :: (Double, Double)
  , bottomRight :: (Double, Double)
  }
  deriving (Show)

viewPortWidth :: ViewPort -> Double
viewPortWidth (ViewPort (x0, _) (x1, _)) = x1 - x0

viewPortHeight :: ViewPort -> Double
viewPortHeight (ViewPort (_, y0) (_, y1)) = y1 - y0

--
-- HitEvent
--

data HitEvent e = HitEvent
  { hitEventHoverOn :: Maybe e
  , hitEventHoverOff :: Maybe e
  , hitEventClick :: Maybe (Either e e)
  -- ^ Left: on -> off, Right: off -> on. If no toggle state, it's always Right.
  }
  deriving (Show)

instance Functor HitEvent where
  fmap f (HitEvent mx my mz) = HitEvent (fmap f mx) (fmap f my) (fmap (bimap f f) mz)

data Rectangle = Rectangle
  { rectXY :: (Double, Double)
  -- ^ (x, y)
  , rectWidth :: Double
  -- ^ w
  , rectHeight :: Double
  -- ^ h
  , rectLineColor :: Maybe Color
  -- ^ line_color
  , rectFillColor :: Maybe Color
  -- ^ fill_color
  , rectLineWidth :: Maybe Double
  -- ^ line_width
  }
  deriving (Show)

data Polyline = Polyline
  { plineStart :: (Double, Double)
  -- ^ start
  , plineBends :: [(Double, Double)]
  -- ^ bend_point
  , plineEnd :: (Double, Double)
  -- ^ end
  , plineColor :: Color
  -- ^ line_color
  , plineWidth :: Double
  -- ^ line_width
  }
  deriving (Show)

data DrawText = DrawText
  { dtextXY :: (Double, Double)
  -- ^ (x, y)
  , dtextScheme :: TextPosition
  -- ^ text_pos
  , dtextFont :: TextFontFace
  -- ^ font_face
  , dtextColor :: Color
  -- ^ font_color
  , dtextFontSize :: Int
  -- ^ font_size
  , dtextContent :: Text
  -- ^ text
  }
  deriving (Show)

data Shape
  = SRectangle Rectangle
  | SPolyline Polyline
  | SDrawText DrawText
  deriving (Show)

data Primitive e = Primitive
  { primShape :: Shape
  , primBoundingBox :: ViewPort
  , primHitEvent :: Maybe (HitEvent e)
  }
  deriving (Show, Functor)

rectangle :: (Double, Double) -> Double -> Double -> Maybe Color -> Maybe Color -> Maybe Double -> Maybe (HitEvent e) -> Primitive e
rectangle (x, y) w h line_color fill_color line_width hitEvent =
  Primitive
    (SRectangle $ Rectangle (x, y) w h line_color fill_color line_width)
    (ViewPort (x, y) (x + w, y + h))
    hitEvent

polyline :: (Double, Double) -> [(Double, Double)] -> (Double, Double) -> Color -> Double -> Primitive e
polyline start bends end color width =
  Primitive
    (SPolyline $ Polyline start bends end color width)
    (ViewPort start end) -- TODO: this is not correct
    Nothing

drawText :: (Double, Double) -> TextPosition -> TextFontFace -> Color -> Int -> Text -> Primitive e
drawText (x, y) text_pos font_face font_color font_size txt =
  Primitive
    (SDrawText $ DrawText (x, y) text_pos font_face font_color font_size txt)
    (ViewPort (x, y) (x + 120, y + fromIntegral font_size + 3)) -- TODO: this is not correct at all
    Nothing

-- scene has local view port matched with global canvas
data Scene e = Scene
  { sceneId :: Text
  , sceneGlobalViewPort :: ViewPort
  , sceneLocalViewPort :: ViewPort
  , sceneElements :: [Primitive e]
  }
  deriving (Show, Functor)

data EventMap e = EventMap
  { eventMapId :: Text
  , eventMapGlobalViewPort :: ViewPort
  , eventMapLocalViewPort :: ViewPort
  , eventMapElements :: [(HitEvent e, ViewPort)]
  }
  deriving (Show, Functor)
