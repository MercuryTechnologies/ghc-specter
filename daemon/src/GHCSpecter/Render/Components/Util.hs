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

moveBoundingBoxBy :: (Double, Double) -> ViewPort -> ViewPort
moveBoundingBoxBy (dx, dy) (ViewPort (vx0, vy0) (vx1, vy1)) =
  ViewPort (vx0 + dx, vy0 + dy) (vx1 + dx, vy1 + dy)

viewPortSum :: Maybe ViewPort -> ViewPort -> ViewPort
viewPortSum Nothing vp = vp
viewPortSum (Just (ViewPort (x0, y0) (x1, y1))) (ViewPort (x0', y0') (x1', y1')) =
  let x0'' = min x0 x0'
      y0'' = min y0 y0'
      x1'' = max x1 x1'
      y1'' = max y1 y1'
   in ViewPort (x0'', y0'') (x1'', y1'')

-- | place grouped items horizontally
flowInline ::
  -- | initial x offset
  Double ->
  -- | rendered items grouped by each x
  [NonEmpty (Primitive e)] ->
  -- | (bounding box of the collection, placed items)
  (Maybe ViewPort, [NonEmpty (Primitive e)])
flowInline offset0 itms0 = (mvp1, itms1)
  where
    place (!offset, !mvp) itms =
      let itms' = fmap forEach itms
          vp' =
            let vps = fmap primBoundingBox itms'
                tl = (minimum $ fmap (fst . topLeft) vps, minimum $ fmap (snd . topLeft) vps)
                br = (maximum $ fmap (fst . bottomRight) vps, maximum $ fmap (snd . bottomRight) vps)
             in ViewPort tl br
          w' = viewPortWidth vp'
       in ((offset + w', Just (viewPortSum mvp vp')), itms')
      where
        forEach (Primitive shape vp hitEvent) =
          let shape' = moveShapeBy (offset, 0) shape
              vp' = moveBoundingBoxBy (offset, 0) vp
           in Primitive shape' vp' hitEvent

    ((_, mvp1), itms1) = L.mapAccumL place (offset0, Nothing) itms0

-- | place grouped items line by line
flowLineByLine ::
  -- | initial y offset
  Double ->
  -- | rendered items grouped by each line
  [NonEmpty (Primitive e)] ->
  -- | (bounding box of the collection, placed items)
  (Double, [NonEmpty (Primitive e)])
flowLineByLine offset0 = L.mapAccumL place offset0
  where
    place !offset itms =
      let shifted = fmap forEach itms
          itms' = fmap snd shifted
          doffset = maximum (fmap fst shifted)
       in (offset + doffset, itms')
      where
        forEach (Primitive shape vp hitEvent) =
          let shape' = moveShapeBy (0, offset) shape
              vp' = moveBoundingBoxBy (0, offset) vp
              doffset = viewPortHeight vp
           in (doffset, Primitive shape' vp' hitEvent)
