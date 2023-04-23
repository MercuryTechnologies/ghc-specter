{-# LANGUAGE OverloadedStrings #-}

module Render.Util.Rules (
  -- * drawing rules
  hruleTop,
  vruleLeft,
  boxRules,

  -- * fill area
  boxFill,
) where

import Control.Monad.Trans.Class (lift)
import GHCSpecter.Graphics.DSL (
  Color (..),
  ViewPort (..),
 )
import GI.Cairo.Render qualified as R
import Renderer (setColor)
import Types (GtkRender)

hruleTop :: ViewPort -> GtkRender e ()
hruleTop (ViewPort (cx0, cy0) (cx1, _)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.moveTo cx0 cy0
  R.lineTo cx1 cy0
  R.stroke

vruleLeft :: ViewPort -> GtkRender e ()
vruleLeft (ViewPort (cx0, cy0) (_cx1, cy1)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.moveTo cx0 cy0
  R.lineTo cx0 cy1
  R.stroke

boxRules :: ViewPort -> GtkRender e ()
boxRules (ViewPort (cx0, cy0) (cx1, cy1)) = lift $ do
  R.setSourceRGBA 0 0 0 1
  R.setLineWidth 1.0
  R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
  R.stroke

boxFill :: Color -> ViewPort -> GtkRender a ()
boxFill color (ViewPort (cx0, cy0) (cx1, cy1)) = do
  setColor color
  lift $ do
    R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
    R.fill
