module GHCSpecter.Util.Transformation (
  -- * transformation function for viewport
  transformScroll,
  transformZoom,
  translateToOrigin,

  -- * hit test
  isInside,
  hitScene,
  hitItem,
) where

import Data.List qualified as L
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (EventMap (..), ViewPort (..))
import GHCSpecter.UI.Types.Event (ScrollDirection (..))

-- | scroll
transformScroll ::
  ScrollDirection ->
  Double ->
  (Double, Double) ->
  ViewPort ->
  ViewPort
transformScroll dir scale (dx, dy) (ViewPort (x0, y0) (x1, y1)) =
  let dx' = dx / scale
      dy' = dy / scale
   in case dir of
        ScrollDirectionRight -> ViewPort (x0 + dx', y0) (x1 + dx', y1)
        ScrollDirectionLeft -> ViewPort (x0 - dx', y0) (x1 - dx', y1)
        ScrollDirectionDown -> ViewPort (x0, y0 + dy') (x1, y1 + dy')
        ScrollDirectionUp -> ViewPort (x0, y0 - dy') (x1, y1 - dy')

-- | zoom
transformZoom ::
  (Double, Double) ->
  Double ->
  ViewPort ->
  ViewPort
transformZoom (rx, ry) scale (ViewPort (x0, y0) (x1, y1)) = ViewPort (x0', y0') (x1', y1')
  where
    x = x0 + (x1 - x0) * rx
    y = y0 + (y1 - y0) * ry
    x0' = x + (x0 - x) / scale
    y0' = y + (y0 - y) / scale
    x1' = x + (x1 - x) / scale
    y1' = y + (y1 - y) / scale

translateToOrigin :: ViewPort -> ViewPort
translateToOrigin (ViewPort (x0, y0) (x1, y1)) = ViewPort (0, 0) (x1 - x0, y1 - y0)

isInside :: (Double, Double) -> ViewPort -> Bool
isInside (x, y) (ViewPort (x0, y0) (x1, y1)) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

hitScene :: (Double, Double) -> [EventMap] -> Maybe EventMap
hitScene (x, y) emaps = L.find (\emap -> (x, y) `isInside` eventMapGlobalViewPort emap) emaps

hitItem :: (Double, Double) -> EventMap -> Maybe Text
hitItem (x, y) emap
  | (x, y) `isInside` cvp =
      fst
        <$> L.find (\(_label, box) -> (vx, vy) `isInside` box) (eventMapElements emap)
  | otherwise =
      Nothing
  where
    cvp@(ViewPort (cx0, cy0) (cx1, cy1)) = eventMapGlobalViewPort emap
    ViewPort (vx0, vy0) (vx1, vy1) = eventMapLocalViewPort emap
    rx = (x - cx0) / (cx1 - cx0)
    ry = (y - cy0) / (cy1 - cy0)
    vx = vx0 + rx * (vx1 - vx0)
    vy = vy0 + ry * (vy1 - vy0)
