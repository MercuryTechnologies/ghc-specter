{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.TimingView
  ( render,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Data.List qualified as L
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Graphics.DSL
  ( Scene (..),
    Stage (..),
    ViewPort (..),
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants
  ( timingRangeHeight,
    timingWidth,
  )
import GHCSpecter.UI.Types
  ( TimingUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import ImGui qualified
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI (windowFlagsScroll)
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  renderState <- mkRenderState
  let stage_ref :: TVar Stage
      stage_ref = renderState.currSharedState.sharedStage
  Stage stage <- liftIO $ atomically $ readTVar stage_ref
  for_ (L.find ((== "timing-chart") . sceneId) stage) $ \stageTiming ->
    for_ (L.find ((== "mem-chart") . sceneId) stage) $ \stageMemory ->
      for_ (L.find ((== "timing-range") . sceneId) stage) $ \stageRange -> do
        let ViewPort (_, vy0) (_, vy1) = stageTiming.sceneLocalViewPort
        runImRender renderState $ do
          renderComponent
            True
            TimingEv
            ( do
                scene <- TimingView.buildTimingChart drvModMap tui ttable
                let scene' =
                      scene
                        { sceneGlobalViewPort = stageTiming.sceneGlobalViewPort,
                          sceneLocalViewPort = stageTiming.sceneLocalViewPort
                        }
                pure scene'
            )
          renderComponent
            False
            TimingEv
            ( do
                scene <- TimingView.buildMemChart False 100 drvModMap tui ttable
                let scene' =
                      scene
                        { sceneGlobalViewPort = stageMemory.sceneGlobalViewPort,
                          sceneLocalViewPort = ViewPort (0, vy0) (0.15 * timingWidth, vy1)
                        }
                pure scene'
            )
          renderComponent
            False
            TimingEv
            ( do
                let scene = TimingView.buildTimingRange tui ttable
                    scene' =
                      scene
                        { sceneGlobalViewPort = stageRange.sceneGlobalViewPort,
                          sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
                        }
                pure scene'
            )
  for_ mhoveredMod $ \hoveredMod -> do
    -- blocker
    renderBlocker hoveredMod ttable
  where
    drvModMap = ss._serverDriverModuleMap
    tui = ui._uiModel._modelTiming
    mhoveredMod = tui._timingUIHoveredModule

    ttable = ss._serverTiming._tsTimingTable

renderBlocker :: ModuleName -> TimingTable -> ReaderT (SharedState UserEvent) IO ()
renderBlocker hoveredMod ttable = do
  liftIO $ do
    v0 <- ImGui.getWindowPos
    h <- ImGui.getWindowHeight
    x0 <- imVec2_x_get v0
    y0 <- imVec2_y_get v0
    zerovec <- ImGui.newImVec2 0 0
    pos <- ImGui.newImVec2 (x0 + 20) (y0 + h - 200)
    ImGui.setNextWindowPos pos 0 zerovec
    delete zerovec
    delete pos
  vec1 <- liftIO $ ImGui.newImVec2 250 180
  _ <- liftIO $ ImGui.beginChild ("blocker_window" :: CString) vec1 (fromBool True) windowFlagsScroll
  liftIO $ delete vec1
  renderState' <- mkRenderState
  runImRender renderState' $
    renderComponent False TimingEv (TimingView.buildBlockers hoveredMod ttable)
  liftIO ImGui.endChild
