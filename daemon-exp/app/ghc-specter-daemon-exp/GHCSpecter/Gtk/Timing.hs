{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Timing (
  renderTiming,
) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.List qualified as L
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Data.Map (BiKeyMap)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (
  Scene (..),
  Stage (..),
  ViewPort (..),
 )
import GHCSpecter.Gtk.Renderer (render)
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.UI.Components.TimingView (
  buildBlockers,
  buildMemChart,
  buildTimingChart,
  buildTimingRange,
 )
import GHCSpecter.UI.Constants (
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  TimingUI,
 )
import GHCSpecter.UI.Types.Event (Event (..))

renderTiming ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  GtkRender Event ()
renderTiming drvModMap tui ttable = do
  stageRef <- vbStage <$> ask
  Stage stage <- liftIO $ atomically $ readTVar stageRef
  for_ (L.find ((== "timing-chart") . sceneId) stage) $ \scene0 -> do
    let ViewPort (_, vy0) (_, vy1) = sceneLocalViewPort scene0
    -- timing chart
    sceneTimingChart <- fmap (fmap TimingEv) <$> buildTimingChart drvModMap tui ttable
    let sceneTimingChart' =
          sceneTimingChart
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneTimingChart'
    -- mem chart
    for_ (L.find ((== "mem-chart") . sceneId) stage) $ \scene1 -> do
      sceneMemChart <- buildMemChart drvModMap tui ttable
      let sceneMemChart' =
            sceneMemChart
              { sceneGlobalViewPort = sceneGlobalViewPort scene1
              , sceneLocalViewPort = ViewPort (0, vy0) (300, vy1)
              }
      render sceneMemChart'
    -- timing range bar
    for_ (L.find ((== "timing-range") . sceneId) stage) $ \scene1 -> do
      let sceneTimingRange = buildTimingRange tui ttable
          sceneTimingRange' =
            sceneTimingRange
              { sceneGlobalViewPort = sceneGlobalViewPort scene1
              , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
              }
      render sceneTimingRange'
    -- blocker lines
    let minfo = do
          hoveredMod <- tui ^. timingUIHoveredModule
          vpCvs <- sceneGlobalViewPort <$> L.find ((== "blockers") . sceneId) stage
          pure (hoveredMod, vpCvs)
    -- NOTE: the size information from vpCvs is ignored as dynamic size overrides it.
    -- TODO: scene content intrinsic size should be present in Scene data type.
    for_ minfo $ \(hoveredMod, vpCvs) -> do
      sceneBlockers <- buildBlockers hoveredMod ttable
      let ViewPort (offsetX, offsetY) _ = vpCvs
          ViewPort (vx0', vy0') (vx1', vy1') = sceneLocalViewPort sceneBlockers
          w = vx1' - vx0'
          h = vy1' - vy0'
          sceneBlockers' =
            sceneBlockers
              { sceneGlobalViewPort = ViewPort (offsetX, offsetY) (w + offsetX, h + offsetY)
              }
      render sceneBlockers'
