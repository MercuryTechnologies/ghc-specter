{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Common
  ( renderComponent,
    withStage,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Text (Text)
import GHCSpecter.Graphics.DSL
  ( Primitive,
    Scene (..),
    Stage (..),
    ViewPort (..),
  )
import GHCSpecter.Layouter.Text (MonadTextLayout (..))
import GHCSpecter.UI.Types.Event (UserEvent (..))
import Handler
  ( handleClick,
    handleMove,
    handleScrollOrZoom,
  )
import ImGui
import STD.Deletable (delete)
import Util.Render
  ( ImRender,
    SharedState (..),
    addEventMap,
    buildEventMap,
    renderScene,
  )

--
-- high-level utilties
--

renderComponent ::
  Bool ->
  (e -> UserEvent) ->
  (forall m. (MonadTextLayout m) => m (Scene (Primitive e))) ->
  ImRender UserEvent ()
renderComponent doesHandleScroll toEv buildScene = do
  renderScene scene
  addEventMap emap
  -- canvas space
  dummy_sz <- liftIO $ newImVec2 (realToFrac totalW) (realToFrac totalH)
  liftIO $ dummy dummy_sz
  -- handling event
  handleMove scene.sceneId
  handleClick scene.sceneId
  when doesHandleScroll $
    handleScrollOrZoom scene.sceneId

  liftIO $ delete dummy_sz
  where
    scene :: Scene (Primitive UserEvent)
    scene = fmap toEv <$> runIdentity buildScene
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneGlobalViewPort.topLeft
    (vx1, vy1) = scene.sceneGlobalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

withStage ::
  Text ->
  (Scene () -> StateT (SharedState UserEvent) IO a) ->
  StateT (SharedState UserEvent) IO ()
withStage scene_name action = do
  shared <- get
  let Stage stage = shared.sharedStage
  traverse_ action (L.find ((== scene_name) . sceneId) stage)
