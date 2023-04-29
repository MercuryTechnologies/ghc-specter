module GHCSpecter.Layouter.Box.Flow (
  -- * shift
  moveShapeBy,
  moveBoundingBoxBy,
  movePrimitiveBy,

  -- * flow
  toSizedLine,
  flowInline,
  flowLineByLine,
) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import GHCSpecter.Graphics.DSL (
  DrawText (..),
  Polyline (..),
  Primitive (..),
  Rectangle (..),
  Shape (..),
  ViewPort (..),
  getLeastUpperBoundingBox,
  viewPortHeight,
  viewPortSum,
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

moveBoundingBoxBy :: (Double, Double) -> ViewPort -> ViewPort
moveBoundingBoxBy (dx, dy) (ViewPort (vx0, vy0) (vx1, vy1)) =
  ViewPort (vx0 + dx, vy0 + dy) (vx1 + dx, vy1 + dy)

movePrimitiveBy :: (Double, Double) -> Primitive e -> Primitive e
movePrimitiveBy (dx, dy) (Primitive shape vp hitEvent) =
  let shape' = moveShapeBy (dx, dy) shape
      vp' = moveBoundingBoxBy (dx, dy) vp
   in Primitive shape' vp' hitEvent

toSizedLine :: NonEmpty (Primitive a) -> (ViewPort, NonEmpty (Primitive a))
toSizedLine xs = (getLeastUpperBoundingBox xs, xs)

-- | place grouped items horizontally
flowInline ::
  -- | initial x offset
  Double ->
  -- | rendered items grouped by each x
  NonEmpty (NonEmpty (Primitive e)) ->
  -- | (bounding box of the collection, placed items)
  (ViewPort, NonEmpty (NonEmpty (Primitive e)))
flowInline offset0 itmss0 = (vp1, itmss1)
  where
    -- the bounding box of the very first item after shifted
    -- just to have the initial vp value.
    vp0 = primBoundingBox $ movePrimitiveBy (offset0, 0) (NE.head (NE.head itmss0))

    go (!offset, !vp) itms =
      let itms' = fmap (movePrimitiveBy (offset, 0)) itms
          vp' = getLeastUpperBoundingBox itms'
          w' = viewPortWidth vp'
       in ((offset + w', viewPortSum vp vp'), itms')

    ((_, vp1), itmss1) = L.mapAccumL go (offset0, vp0) itmss0

-- | place grouped items line by line
flowLineByLine ::
  -- | initial y offset
  Double ->
  -- | rendered items grouped by each line
  NonEmpty (ViewPort, NonEmpty (Primitive e)) ->
  -- | (final offset, placed items)
  (ViewPort, NonEmpty (NonEmpty (Primitive e)))
flowLineByLine offset0 itmss0 = (vp1, itmss1)
  where
    -- the bounding box of the very first item after shifted
    -- just to have the initial vp value.
    vp0 = moveBoundingBoxBy (0, offset0) $ fst (NE.head itmss0)

    go (!offset, !vp_) (vp, itms) =
      let itms' = fmap (movePrimitiveBy (0, offset)) itms
          vp' = moveBoundingBoxBy (0, offset) vp
          h' = viewPortHeight vp'
       in ((offset + h', viewPortSum vp_ vp'), itms')

    ((_, vp1), itmss1) = L.mapAccumL go (offset0, vp0) itmss0
