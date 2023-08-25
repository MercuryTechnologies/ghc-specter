{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.TimingView
  ( renderTimingView,
    renderMemoryView,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Graphics.DSL
  ( ViewPort (..),
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
import Render.Common (renderComponent)
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

renderTimingView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderTimingView ui ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent TimingEv (TimingView.buildTimingChart drvModMap tui' ttable)
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

renderMemoryView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMemoryView ui ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent TimingEv (TimingView.buildMemChart False 200 drvModMap tui' ttable)
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