module Render.Common (vruleLeft) where

import GHCSpecter.Graphics.DSL (
  ViewPort (..),
 )
import GI.Cairo.Render qualified as R

vruleLeft :: ViewPort -> R.Render ()
vruleLeft (ViewPort (cx0, cy0) (_cx1, cy1)) = do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.moveTo cx0 cy0
  R.lineTo cx0 cy1
  R.stroke
