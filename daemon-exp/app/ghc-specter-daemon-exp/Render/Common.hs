{-# LANGUAGE OverloadedStrings #-}

module Render.Common (
  -- * drawing rules
  hruleTop,
  vruleLeft,
  boxRules,

  -- * conversion
  convertTopLevelTab,
) where

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import GHCSpecter.Graphics.DSL (
  Scene,
  ViewPort (..),
 )
import GHCSpecter.UI.Types.Event (Event (..), Tab (..))
import GI.Cairo.Render qualified as R
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

-- TODO: generalize compileTab further so that we do not need this.
convertTopLevelTab :: Scene (Text, Int) -> Scene Event
convertTopLevelTab s = fmap f s
  where
    f (txt, _)
      | txt == "TabSession" = TabEv TabSession
      | txt == "TabModuleGraph" = TabEv TabModuleGraph
      | txt == "TabSourceView" = TabEv TabSourceView
      | txt == "TabTiming" = TabEv TabTiming
      | otherwise = DummyEv
