module GHCSpecter.Render.Components.Util (
  flowLineByLine,
) where

import Data.List qualified as L
import GHCSpecter.Graphics.DSL (
  Primitive (..),
 )

flowLineByLine ::
  -- | initial y offset
  Double ->
  -- | rendered items grouped by each line
  [[Primitive e]] ->
  -- | (final offset after placement, placed items)
  (Double, [[Primitive e]])
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
