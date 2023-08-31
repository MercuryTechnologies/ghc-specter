module GHCSpecter.Util.Transformation
  ( isValid,

    -- * transformation function for viewport
    transformScroll,
    transformZoom,
    translateToOrigin,

    -- * hit test
    hitScene,
    hitItem,
  )
where

import Data.List qualified as L
import GHCSpecter.Graphics.DSL
  ( EventMap,
    HitEvent,
    ViewPort (..),
    eventMapElements,
    eventMapGlobalViewPort,
    eventMapLocalViewPort,
    isInside,
  )

isValid :: ViewPort -> Bool
isValid (ViewPort (x0, y0) (x1, y1)) = x0 <= x1 && y0 <= y1

-- | scroll
transformScroll ::
  Maybe ViewPort ->
  Double ->
  (Double, Double) ->
  ViewPort ->
  ViewPort
transformScroll vpLimit scale (dx, dy) vp = vp''
  where
    ViewPort (x0, y0) (x1, y1) = vp
    dx' = dx / scale
    dy' = dy / scale

    vp'' =
      let (x0', y0') = (x0 - dx', y0 - dy')
          (x1', y1') = (x1 - dx', y1 - dy')
          vp' = ViewPort (x0', y0') (x1', y1')
          eps = 1.0
       in case vpLimit of
            Nothing -> vp'
            Just (ViewPort (xL0, yL0) (xL1, yL1)) ->
              let (x0'', x1'')
                    | abs dx' < eps || x1' < xL0 || x0' > xL1 = (x0, x1)
                    | otherwise = (x0', x1')
                  (y0'', y1'')
                    | abs dy' < eps || y1' < yL0 || y0' > yL1 = (y0, y1)
                    | otherwise = (y0', y1')
               in ViewPort (x0'', y0'') (x1'', y1'')

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
