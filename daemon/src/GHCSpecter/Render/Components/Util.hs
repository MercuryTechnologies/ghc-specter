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
 )

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
        forEachShape (SDrawText (txt@DrawText {})) =
          let
            -- this should use proper layout engine
            doffset = 120
            (x, y) = dtextXY txt
           in
            (doffset, SDrawText (txt {dtextXY = (x + offset, y)}))
        forEachShape (SPolyline (poly@Polyline {})) =
          let (x0, y0) = plineStart poly
              xs = plineBends poly
              (x1, y1) = plineEnd poly
              doffset = x1 - x0
              f (x, y) = (x + offset, y)
              poly' =
                poly
                  { plineStart = f (x0, y0)
                  , plineBends = fmap f xs
                  , plineEnd = f (x1, y1)
                  }
           in (doffset, SPolyline poly')
        forEachShape (SRectangle (rect@Rectangle {})) =
          let (x, y) = rectXY rect
              doffset = rectWidth rect
           in (doffset, SRectangle (rect {rectXY = (x + offset, y)}))

        moveBoundingBox (ViewPort (vx0, vy0) (vx1, vy1)) =
          ViewPort (vx0 + offset, vy0) (vx1 + offset, vy1)

        forEach (Primitive shape vp hitEvent) =
          let (doffset, shape') = forEachShape shape
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
        forEachShape (SDrawText (txt@DrawText {})) =
          let (x, y) = dtextXY txt
              fs = dtextFontSize txt
              doffset = fromIntegral fs + 4
           in (doffset, SDrawText (txt {dtextXY = (x, y + offset)}))
        forEachShape (SPolyline (poly@Polyline {})) =
          let doffset = 5
              (x0, y0) = plineStart poly
              xs = plineBends poly
              (x1, y1) = plineEnd poly
              f (x, y) = (x, y + offset + 3)
              poly' =
                poly
                  { plineStart = f (x0, y0)
                  , plineBends = fmap f xs
                  , plineEnd = f (x1, y1)
                  }
           in (doffset, SPolyline poly')
        forEachShape (SRectangle (rect@Rectangle {})) =
          let (x, y) = rectXY rect
              doffset = rectHeight rect
           in (doffset, SRectangle (rect {rectXY = (x, y + offset)}))

        moveBoundingBox (ViewPort (vx0, vy0) (vx1, vy1)) =
          ViewPort (vx0, vy0 + offset) (vx1, vy1 + offset)

        forEach (Primitive shape vp hitEvent) =
          let (doffset, shape') = forEachShape shape
              vp' = moveBoundingBox vp
           in (doffset, Primitive shape' vp' hitEvent)
