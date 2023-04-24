module GHCSpecter.Render.Components.Util (
  flowInline,
  flowLineByLine,
) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import GHCSpecter.Graphics.DSL (
  DrawText (..),
  Polyline (..),
  Primitive (..),
  Rectangle (..),
  Shape (..),
  ViewPort (..),
  viewPortHeight,
  viewPortWidth,
 )

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

-- | place grouped items horizontally
flowInline ::
  -- | initial x offset
  Double ->
  -- | rendered items grouped by each x
  [NonEmpty (Primitive e)] ->
  -- | (final offset after placement, placed itmes)
  (Double, [NonEmpty (Primitive e)])
flowInline offset0 = L.mapAccumL place offset0
  where
    place !offset itms =
      let shifted = fmap forEach itms
          itms' = fmap snd shifted
          doffset = maximum (fmap fst shifted)
       in (offset + doffset, itms')
      where
        moveBoundingBox (ViewPort (vx0, vy0) (vx1, vy1)) =
          ViewPort (vx0 + offset, vy0) (vx1 + offset, vy1)

        forEach (Primitive shape vp hitEvent) =
          let shape' = moveShapeBy (offset, 0) shape
              doffset = viewPortWidth vp
              vp' = moveBoundingBox vp
           in (doffset, Primitive shape' vp' hitEvent)

-- | place grouped items line by line
flowLineByLine ::
  -- | initial y offset
  Double ->
  -- | rendered items grouped by each line
  [NonEmpty (Primitive e)] ->
  -- | (final offset after placement, placed items)
  (Double, [NonEmpty (Primitive e)])
flowLineByLine offset0 = L.mapAccumL place offset0
  where
    place !offset itms =
      let shifted = fmap forEach itms
          itms' = fmap snd shifted
          doffset = maximum (fmap fst shifted)
       in (offset + doffset, itms')
      where
        moveBoundingBox (ViewPort (vx0, vy0) (vx1, vy1)) =
          ViewPort (vx0, vy0 + offset) (vx1, vy1 + offset)

        forEach (Primitive shape vp hitEvent) =
          let shape' = moveShapeBy (0, offset) shape
              doffset = viewPortHeight vp
              vp' = moveBoundingBox vp
           in (doffset, Primitive shape' vp' hitEvent)
