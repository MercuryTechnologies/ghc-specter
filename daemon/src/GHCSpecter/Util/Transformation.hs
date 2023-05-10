module GHCSpecter.Util.Transformation (
  isValid,

  -- * transformation function for viewport
  transformScroll,
  transformZoom,
  translateToOrigin,

  -- * hit test
  hitScene,
  hitItem,
) where

import Data.List qualified as L
import Debug.Trace (trace)
import GHCSpecter.Graphics.DSL (
  EventMap,
  HitEvent,
  ViewPort (..),
  eventMapElements,
  eventMapGlobalViewPort,
  eventMapLocalViewPort,
  isInside,
 )
import GHCSpecter.UI.Types.Event (ScrollDirection (..))

isValid :: ViewPort -> Bool
isValid (ViewPort (x0, y0) (x1, y1)) = x0 <= x1 && y0 <= y1

-- either one of the ranges are completely inside the other
isOneRangeEmbeddedInTheOther :: (Double, Double) -> (Double, Double) -> Bool
isOneRangeEmbeddedInTheOther (x0, x1) (x0', x1') =
  -- (x0, x1) includes (x0', x1')
  ((x0 <= x0') && (x1 >= x1'))
    -- (x0', x1') includes (x0, x1)
    || ((x0' <= x0) && (x1' >= x1))

isOneViewPortEmbeddedInTheOther :: ViewPort -> ViewPort -> Bool
isOneViewPortEmbeddedInTheOther (ViewPort (x0, y0) (x1, y1)) (ViewPort (x0', y0') (x1', y1')) =
  isOneRangeEmbeddedInTheOther (x0, x1) (x0', x1') && isOneRangeEmbeddedInTheOther (y0, y1) (y0', y1')

-- | scroll
transformScroll ::
  Maybe ViewPort ->
  ScrollDirection ->
  Double ->
  (Double, Double) ->
  ViewPort ->
  ViewPort
transformScroll mvpLimit dir scale (dx, dy) vp = vp''
  where
    ViewPort (x0, y0) (x1, y1) = vp
    dx' = dx / scale
    dy' = dy / scale

    vp' = case dir of
      ScrollDirectionRight ->
        ViewPort (x0 + dx', y0) (x1 + dx', y1)
      ScrollDirectionLeft ->
        ViewPort (x0 - dx', y0) (x1 - dx', y1)
      ScrollDirectionDown ->
        ViewPort (x0, y0 + dy') (x1, y1 + dy')
      ScrollDirectionUp ->
        ViewPort (x0, y0 - dy') (x1, y1 - dy')
      ScrollDirectionSmooth ->
        ViewPort (x0 + dx', y0 + dy') (x1 + dx', y1 + dy')
    vp'' = case mvpLimit of
      Just vpLimit
        | isOneViewPortEmbeddedInTheOther vpLimit vp && isOneViewPortEmbeddedInTheOther vpLimit vp' -> vp'
        | isOneViewPortEmbeddedInTheOther vpLimit vp -> vp
        | otherwise -> vp'
      Nothing -> vp'

-- | zoom
transformZoom ::
  (Double, Double) ->
  Double ->
  ViewPort ->
  ViewPort
transformZoom (rx, ry) scale vp = vp'
  where
    ViewPort (x0, y0) (x1, y1) = vp
    x = x0 + (x1 - x0) * rx
    y = y0 + (y1 - y0) * ry
    x0' = x + (x0 - x) / scale
    y0' = y + (y0 - y) / scale
    x1' = x + (x1 - x) / scale
    y1' = y + (y1 - y) / scale
    vp' = ViewPort (x0', y0') (x1', y1')

translateToOrigin :: ViewPort -> ViewPort
translateToOrigin (ViewPort (x0, y0) (x1, y1)) = ViewPort (0, 0) (x1 - x0, y1 - y0)

hitScene :: (Double, Double) -> [EventMap e] -> Maybe (EventMap e)
hitScene (x, y) emaps = L.find (\emap -> (x, y) `isInside` eventMapGlobalViewPort emap) emaps

hitItem :: (Double, Double) -> EventMap e -> Maybe (HitEvent e)
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
