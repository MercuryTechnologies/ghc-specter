module GHCSpecter.Render.Components.Util (
  -- * ViewPort util
  getLeastUpperBoundingBox,

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

getLeastUpperBoundingBox :: NonEmpty (Primitive e) -> ViewPort
getLeastUpperBoundingBox itms =
  let vps = fmap primBoundingBox itms
      tl = (minimum $ fmap (fst . topLeft) vps, minimum $ fmap (snd . topLeft) vps)
      br = (maximum $ fmap (fst . bottomRight) vps, maximum $ fmap (snd . bottomRight) vps)
   in ViewPort tl br

toSizedLine :: NonEmpty (Primitive a) -> (ViewPort, NonEmpty (Primitive a))
toSizedLine xs = (getLeastUpperBoundingBox xs, xs)

-- for now
viewPortSum' = viewPortSum . Just

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
    vp0 = primBoundingBox $ shift offset0 (NE.head (NE.head itmss0))

    go (!offset, !vp) itms =
      let itms' = fmap (shift offset) itms
          vp' = getLeastUpperBoundingBox itms'
          w' = viewPortWidth vp'
       in ((offset + w', viewPortSum' vp vp'), itms')
    shift offset (Primitive shape vp hitEvent) =
      let shape' = moveShapeBy (offset, 0) shape
          vp' = moveBoundingBoxBy (offset, 0) vp
       in Primitive shape' vp' hitEvent

    ((_, vp1), itmss1) = L.mapAccumL go (offset0, vp0) itmss0

-- | place grouped items line by line
flowLineByLine ::
  -- | initial y offset
  Double ->
  -- | rendered items grouped by each line
  NonEmpty (ViewPort, NonEmpty (Primitive e)) ->
  -- | (final offset, placed items)
  (Maybe ViewPort, NonEmpty (NonEmpty (Primitive e)))
flowLineByLine offset0 itmss0 = (mvp1, itmss1)
  where
    -- the bounding box of the very first item after shifted
    -- just to have the initial vp value.
    vp0 = moveBoundingBoxBy (0, offset0) $ fst (NE.head itmss0)
    
    go (!offset, !mvp) (vp, itms) =
      let itms' = fmap (shift offset) itms
          vp' = moveBoundingBoxBy (0, offset) vp
          h' = viewPortHeight vp'
       in ((offset + h', Just (viewPortSum mvp vp')), itms')
    shift offset (Primitive shape vp_ hitEvent) =
      let shape' = moveShapeBy (0, offset) shape
          vp_' = moveBoundingBoxBy (0, offset) vp_
       in Primitive shape' vp_' hitEvent

    ((_, mvp1), itmss1) = L.mapAccumL go (offset0, Just vp0) itmss0
