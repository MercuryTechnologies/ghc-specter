{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Common
  ( renderComponent,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (runIdentity)
import GHCSpecter.Graphics.DSL
  ( Primitive,
    Scene (..),
    ViewPort (..),
  )
import GHCSpecter.Layouter.Text (MonadTextLayout (..))
import GHCSpecter.UI.Types.Event (UserEvent (..))
import Handler (handleClick, handleMove)
import ImGui
import STD.Deletable (delete)
import Util.Render
  ( ImRender,
    addEventMap,
    buildEventMap,
    renderScene,
  )

--
-- high-level utilties
--

renderComponent ::
  (e -> UserEvent) ->
  (forall m. (MonadTextLayout m) => m (Scene (Primitive e))) ->
  ImRender UserEvent ()
renderComponent toEv buildScene = do
  renderScene scene
  addEventMap emap
  -- canvas space
  dummy_sz <- liftIO $ newImVec2 (realToFrac totalW) (realToFrac totalH)
  liftIO $ dummy dummy_sz
  -- handling event
  handleMove scene.sceneId
  handleClick scene.sceneId
  liftIO $ delete dummy_sz
  where
    scene :: Scene (Primitive UserEvent)
    scene = fmap toEv <$> runIdentity buildScene
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0
