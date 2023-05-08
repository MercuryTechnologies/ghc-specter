module GHCSpecter.Graphics.DSL (
  -- * enum types
  Color (..),
  TextFontFace (..),
  TextPosition (..),

  -- * view port
  ViewPort (..),

  -- * ViewPort util
  viewPortWidth,
  viewPortHeight,
  viewPortSum,
  isInside,
  overlapsWith,

  -- * event type
  HitEvent (..),

  -- * graphics primitives
  Rectangle (..),
  Polyline (..),
  DrawText (..),
  Shape (..),
  Primitive (..),

  -- * smart constructors
  rectangle,
  polyline,

  -- * primitive util
  moveShapeBy,
  moveBoundingBoxBy,
  movePrimitiveBy,
  getLeastUpperBoundingBox,

  -- * scene
  Scene (..),
  EventMap,
  eventMapId,
  eventMapGlobalViewPort,
  eventMapLocalViewPort,
  eventMapElements,

  -- * stage
  Stage (..),
) where

import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty)
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

viewPortSum :: ViewPort -> ViewPort -> ViewPort
viewPortSum (ViewPort (x0, y0) (x1, y1)) (ViewPort (x0', y0') (x1', y1')) =
  let x0'' = min x0 x0'
      y0'' = min y0 y0'
      x1'' = max x1 x1'
      y1'' = max y1 y1'
   in ViewPort (x0'', y0'') (x1'', y1'')

isInside :: (Double, Double) -> ViewPort -> Bool
isInside (x, y) (ViewPort (x0, y0) (x1, y1)) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

isInsideR :: Double -> (Double, Double) -> Bool
isInsideR x (x0, x1) = x >= x0 && x <= x1

overlapsWith :: ViewPort -> ViewPort -> Bool
overlapsWith (ViewPort (x0, y0) (x1, y1)) (ViewPort (x0', y0') (x1', y1')) =
  x0'
    `isInsideR` (x0, x1)
    || x1'
    `isInsideR` (x0, x1)
    || x0
    `isInsideR` (x0', x1')
    || x1
    `isInsideR` (x0', x1')
    || y0'
    `isInsideR` (y0, y1)
    || y1'
    `isInsideR` (y0, y1)
    || y0
    `isInsideR` (y0', y1')
    || y1
    `isInsideR` (y0', y1')

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

moveShapeBy :: (Double, Double) -> Shape -> Shape
moveShapeBy (dx, dy) (SDrawText (txt@DrawText {})) =
  let (x, y) = dtextXY txt
   in SDrawText (txt {dtextXY = (x + dx, y + dy)})
moveShapeBy (dx, dy) (SPolyline (poly@Polyline {})) =
  let (x0, y0) = plineStart poly
      xs = plineBends poly
      (x1, y1) = plineEnd poly
      f (x, y) = (x + dx, y + dy)
      poly' =
        poly
          { plineStart = f (x0, y0)
          , plineBends = fmap f xs
          , plineEnd = f (x1, y1)
          }
   in SPolyline poly'
moveShapeBy (dx, dy) (SRectangle (rect@Rectangle {})) =
  let (x, y) = rectXY rect
   in SRectangle (rect {rectXY = (x + dx, y + dy)})

moveBoundingBoxBy :: (Double, Double) -> ViewPort -> ViewPort
moveBoundingBoxBy (dx, dy) (ViewPort (vx0, vy0) (vx1, vy1)) =
  ViewPort (vx0 + dx, vy0 + dy) (vx1 + dx, vy1 + dy)

movePrimitiveBy :: (Double, Double) -> Primitive e -> Primitive e
movePrimitiveBy (dx, dy) (Primitive shape vp hitEvent) =
  let shape' = moveShapeBy (dx, dy) shape
      vp' = moveBoundingBoxBy (dx, dy) vp
   in Primitive shape' vp' hitEvent

getLeastUpperBoundingBox :: NonEmpty (Primitive e) -> ViewPort
getLeastUpperBoundingBox itms =
  let vps = fmap primBoundingBox itms
      tl = (minimum $ fmap (fst . topLeft) vps, minimum $ fmap (snd . topLeft) vps)
      br = (maximum $ fmap (fst . bottomRight) vps, maximum $ fmap (snd . bottomRight) vps)
   in ViewPort tl br

-- scene has local view port matched with global canvas
data Scene elem = Scene
  { sceneId :: Text
  , sceneGlobalViewPort :: ViewPort
  , sceneLocalViewPort :: ViewPort
  , sceneElements :: [elem]
  , sceneExtents :: Maybe ViewPort
  }
  deriving (Show, Functor)

type EventMap e = Scene (HitEvent e, ViewPort)

eventMapId :: EventMap e -> Text
eventMapId = sceneId

eventMapGlobalViewPort :: EventMap e -> ViewPort
eventMapGlobalViewPort = sceneGlobalViewPort

eventMapLocalViewPort :: EventMap e -> ViewPort
eventMapLocalViewPort = sceneLocalViewPort

eventMapElements :: EventMap e -> [(HitEvent e, ViewPort)]
eventMapElements = sceneElements

data Stage = Stage
  { stageElems :: [Scene ()]
  }
