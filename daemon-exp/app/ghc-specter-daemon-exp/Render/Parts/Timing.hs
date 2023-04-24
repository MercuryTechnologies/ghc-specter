{-# LANGUAGE OverloadedStrings #-}

module Render.Parts.Timing (
  renderTiming,
) where

import Control.Lens (to, (^.))
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Data.Map (BiKeyMap)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (
  Scene (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.TimingView (
  buildBlockers,
  buildMemChart,
  buildTimingChart,
  buildTimingRange,
 )
import GHCSpecter.UI.Constants (
  HasWidgetConfig (..),
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasViewPortInfo (..),
  TimingUI,
 )
import GHCSpecter.UI.Types.Event (Event (..))
import Renderer (
  addEventMap,
  renderScene,
 )
import Types (GtkRender, ViewBackend (..))

renderTiming ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  GtkRender Event ()
renderTiming drvModMap tui ttable = do
  wcfg <- (^. to vbWidgetConfig . wcfgTiming) <$> ask
  let vpi = tui ^. timingUIViewPort
      vp@(ViewPort (_, vy0) (_, vy1)) =
        fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
  -- timing chart
  for_ (Map.lookup "timing-chart" wcfg) $ \vpCvs -> do
    let sceneTimingChart = TimingEv <$> buildTimingChart drvModMap tui ttable
        sceneTimingChart' =
          sceneTimingChart
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene sceneTimingChart'
    addEventMap sceneTimingChart'
  -- mem chart
  for_ (Map.lookup "mem-chart" wcfg) $ \vpCvs -> do
    let sceneMemChart = buildMemChart drvModMap tui ttable
        sceneMemChart' =
          sceneMemChart
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = ViewPort (0, vy0) (300, vy1)
            }
    renderScene sceneMemChart'
  -- timing range bar
  for_ (Map.lookup "timing-range" wcfg) $ \vpCvs -> do
    let sceneTimingRange = buildTimingRange tui ttable
        sceneTimingRange' =
          sceneTimingRange
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
            }
    renderScene sceneTimingRange'
  -- blocker lines
  let minfo = do
        hoveredMod <- tui ^. timingUIHoveredModule
        vpCvs <- Map.lookup "blockers" wcfg
        pure (hoveredMod, vpCvs)
  -- NOTE: the size information from vpCvs is ignored as dynamic size overrides it.
  -- TODO: scene content intrinsic size should be present in Scene data type.
  for_ minfo $ \(hoveredMod, vpCvs) -> do
    let sceneBlockers = buildBlockers hoveredMod ttable
        ViewPort (offsetX, offsetY) _ = vpCvs
        ViewPort (vx0', vy0') (vx1', vy1') = sceneLocalViewPort sceneBlockers
        w = vx1' - vx0'
        h = vy1' - vy0'
        sceneBlockers' =
          sceneBlockers
            { sceneGlobalViewPort = ViewPort (offsetX, offsetY) (w + offsetX, h + offsetY)
            }
    renderScene sceneBlockers'
