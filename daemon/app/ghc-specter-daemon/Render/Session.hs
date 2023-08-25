{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Session
  ( renderSession,
  )
where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (runIdentity)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
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
import Util.GUI (defTableFlags, windowFlagsScroll)
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
  vec1 <- liftIO $ newImVec2 0 100
  vec2 <- liftIO $ newImVec2 0 (-200)
  vec3 <- liftIO $ newImVec2 0 0
  whenM (toBool <$> liftIO (beginTable ("##table" :: CString) 1 defTableFlags)) $ do
    liftIO $ tableSetupColumn_ ("graph" :: CString)
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    _ <- liftIO $ beginChild ("#session-info" :: CString) vec1 (fromBool False) windowFlagsScroll
    renderSessionInfo ss
    liftIO endChild
    --
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    _ <- liftIO $ beginChild ("#process-info" :: CString) vec2 (fromBool False) windowFlagsScroll
    renderProcessPanel ss
    liftIO endChild
    --
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    _ <- liftIO $ beginChild ("#rts-info" :: CString) vec3 (fromBool False) windowFlagsScroll
    renderRtsPanel ss
    liftIO endChild
    --
    liftIO endTable
  liftIO $ delete vec1
  liftIO $ delete vec2
  liftIO $ delete vec3

renderSessionInfo :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSessionInfo ss = do
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

renderProcessPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderProcessPanel ss = do
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
          <$> runIdentity (Session.buildProcessPanel ss)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

renderRtsPanel :: ServerState -> ReaderT (SharedState UserEvent) IO ()
renderRtsPanel ss = do
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
          <$> runIdentity (Session.buildRtsPanel ss)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0
