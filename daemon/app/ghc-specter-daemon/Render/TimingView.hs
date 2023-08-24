{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.TimingView
  ( renderTimingView,
    renderMemoryView,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
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
import GHCSpecter.UI.Components.GraphView qualified as GraphView
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants (timingMaxWidth)
import GHCSpecter.UI.Types
  ( ModuleGraphUI (..),
    TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import ImGui
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.Render
  ( SharedState (..),
    addEventMap,
    buildEventMap,
    mkRenderState,
    renderScene,
    runImRender,
  )

renderTimingView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderTimingView ui ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      renderScene scene
      addEventMap emap
    -- handleMouseMove (totalW, totalH)
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
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

    scene :: Scene (Primitive UserEvent)
    scene =
      ( fmap TimingEv
          <$> runIdentity (TimingView.buildTimingChart drvModMap tui' ttable)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

renderMemoryView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMemoryView ui ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      renderScene scene
      addEventMap emap
    -- handleMouseMove (totalW, totalH)
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
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

    scene :: Scene (Primitive UserEvent)
    scene = runIdentity $ TimingView.buildMemChart False 200 drvModMap tui' ttable
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0
