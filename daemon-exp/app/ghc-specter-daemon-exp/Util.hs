{-# LANGUAGE OverloadedLabels #-}

module Util (
  -- * transformation function for viewport
  transformScroll,
  transformZoom,

  -- * hit test
  isInside,
) where

import GHCSpecter.UI.Types (ViewPort (..))
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

isInside :: (Double, Double) -> ((Double, Double), (Double, Double)) -> Bool
isInside (x, y) ((x0, y0), (x1, y1)) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1
