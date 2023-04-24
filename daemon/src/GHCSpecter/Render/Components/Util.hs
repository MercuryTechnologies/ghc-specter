module GHCSpecter.Render.Components.Util (
  flowInline,
  flowLineByLine,
) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import GHCSpecter.Graphics.DSL (
  Polyline (..),
  Primitive (..),
  Rectangle (..),
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
        forEach item =
          case item of
            DrawText (x, y) p' ff c fs t ->
              let
                -- this should use proper layout engine
                doffset = 120
               in
                (doffset, DrawText (x + offset, y) p' ff c fs t)
            PPolyline (poly@Polyline {}) ->
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
               in (doffset, PPolyline poly')
            PRectangle (rect@Rectangle {}) ->
              let (x, y) = rectXY rect
                  doffset = rectWidth rect
               in (doffset, PRectangle (rect {rectXY = (x + offset, y)}))

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
        forEach item =
          case item of
            DrawText (x, y) p' ff c fs t ->
              let doffset = fromIntegral fs + 4
               in (doffset, DrawText (x, y + offset) p' ff c fs t)
            PPolyline (poly@Polyline {}) ->
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
               in (doffset, PPolyline poly')
            PRectangle (rect@Rectangle {}) ->
              let (x, y) = rectXY rect
                  doffset = rectHeight rect
               in (doffset, PRectangle (rect {rectXY = (x, y + offset)}))
