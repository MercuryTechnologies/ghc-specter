module GHCSpecter.Render.Components.Util (
  flowInline,
  flowLineByLine,
) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import GHCSpecter.Graphics.DSL (
  Primitive (..),
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
            Polyline (x0, y0) xs (x1, y1) c w ->
              let doffset = x1 - x0
                  f (x, y) = (x + offset, y)
               in (doffset, Polyline (f (x0, y0)) (fmap f xs) (f (x1, y1)) c w)
            Rectangle (x, y) w h ms mf mw me ->
              let doffset = w
               in (doffset, Rectangle (x + offset, y) w h ms mf mw me)

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
            Polyline (x0, y0) xs (x1, y1) c w ->
              let doffset = 5
                  f (x, y) = (x, y + offset + 3)
               in (doffset, Polyline (f (x0, y0)) (fmap f xs) (f (x1, y1)) c w)
            Rectangle (x, y) w h ms mf mw me ->
              let doffset = h
               in (doffset, Rectangle (x, y + offset) w h ms mf mw me)
