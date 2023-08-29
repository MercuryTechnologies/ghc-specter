{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.TimingView
  ( render,
    renderMemoryView,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (for_)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Graphics.DSL
  ( Scene (..),
    ViewPort (..),
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants (timingMaxWidth)
import GHCSpecter.UI.Types
  ( TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import ImGui qualified
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI (windowFlagsScroll)
import Util.Render
  ( ImRender (..),
    ImRenderState (..),
    SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $ do
      renderComponent
        True
        TimingEv
        ( do
            scene <- TimingView.buildTimingChart drvModMap tui' ttable
            pure scene -- pure scene {sceneLocalViewPort = ViewPort (0, 0) (640, 480)}
        )
  for_ mhoveredMod $ \hoveredMod -> do
    -- blocker
    renderBlocker hoveredMod ttable
  where
    drvModMap = ss._serverDriverModuleMap
    tui = ui._uiModel._modelTiming
    mhoveredMod = tui._timingUIHoveredModule

    ttable = ss._serverTiming._tsTimingTable
    timingInfos = ttable._ttableTimingInfos

    nMods = length timingInfos
    totalHeight = 5 * nMods
    vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

    tui' =
      tui
        { _timingUIPartition = True
        -- _timingUIViewPort = ViewPortInfo vp Nothing
        }

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
  liftIO $
    runImRender renderState' $
      renderComponent False TimingEv (TimingView.buildBlockers hoveredMod ttable)
  liftIO ImGui.endChild

renderMemoryView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMemoryView ui ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent False TimingEv (TimingView.buildMemChart False 200 drvModMap tui' ttable)
  where
    drvModMap = ss._serverDriverModuleMap
    tui = ui._uiModel._modelTiming
    ttable = ss._serverTiming._tsTimingTable
    timingInfos = ttable._ttableTimingInfos

    nMods = length timingInfos
    totalHeight = 5 * nMods
    vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

    tui' =
      tui
        { _timingUIPartition = True,
          _timingUIViewPort = ViewPortInfo vp Nothing
        }
