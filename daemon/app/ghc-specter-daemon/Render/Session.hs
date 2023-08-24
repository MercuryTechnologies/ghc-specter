{-# LANGUAGE OverloadedRecordDot #-}

module Render.Session
  ( renderSession,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (runIdentity)
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Graphics.DSL
  ( Primitive,
    Scene (..),
    ViewPort (..),
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Session qualified as Session
import GHCSpecter.UI.Types
  ( TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import Handler (handleClick, handleMove)
import ImGui
import STD.Deletable (delete)
import Util.Render
  ( SharedState (..),
    addEventMap,
    buildEventMap,
    mkRenderState,
    renderScene,
    runImRender,
  )

renderSession :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSession _ui ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
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
    scene =
      ( fmap SessionEv
          <$> runIdentity (Session.buildSession ss)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0
