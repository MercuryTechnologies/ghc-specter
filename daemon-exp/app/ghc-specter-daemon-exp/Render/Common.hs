module Render.Common (
  hruleTop,
  vruleLeft,
  boxRules,
) where

import Control.Monad.Trans.Class (lift)
import GHCSpecter.Graphics.DSL (
  ViewPort (..),
 )
import GI.Cairo.Render qualified as R
import Types (GtkRender)

hruleTop :: ViewPort -> GtkRender ()
hruleTop (ViewPort (cx0, cy0) (cx1, _)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.moveTo cx0 cy0
  R.lineTo cx1 cy0
  R.stroke

vruleLeft :: ViewPort -> GtkRender ()
vruleLeft (ViewPort (cx0, cy0) (_cx1, cy1)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.moveTo cx0 cy0
  R.lineTo cx0 cy1
  R.stroke

boxRules :: ViewPort -> GtkRender ()
boxRules (ViewPort (cx0, cy0) (cx1, cy1)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
  R.stroke
