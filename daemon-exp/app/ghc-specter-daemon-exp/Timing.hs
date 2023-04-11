{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Control.Lens ((.~), (^.))
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
  compileBlockers,
  compileMemChart,
  compileTimingChart,
  compileTimingRange,
 )
import GHCSpecter.UI.Constants (
  timingHeight,
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasUIViewRaw (..),
  HasViewPortInfo (..),
  HasWidgetConfig (..),
  TimingUI,
  UIState,
 )
import GI.Cairo.Render qualified as R
import Renderer (
  addEventMap,
  renderScene,
 )
import Types (ViewBackend)

renderTiming ::
  TVar UIState ->
  ViewBackend ->
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  R.Render ()
renderTiming uiRef vb drvModMap tui ttable = do
  wcfg <-
    R.liftIO $
      atomically $ do
        modifyTVar' uiRef (uiViewRaw . uiRawEventMap .~ [])
        (^. uiModel . modelWidgetConfig . wcfgTiming) <$> readTVar uiRef
  let vpi = tui ^. timingUIViewPort
      vp@(ViewPort (_, vy0) (_, vy1)) =
        fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
  -- timing chart
  for_ (Map.lookup "timing-chart" wcfg) $ \vpCvs -> do
    let sceneTimingChart = compileTimingChart drvModMap tui ttable
        sceneTimingChart' =
          sceneTimingChart
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene vb sceneTimingChart'
    R.liftIO $ addEventMap uiRef sceneTimingChart'
  -- mem chart
  for_ (Map.lookup "mem-chart" wcfg) $ \vpCvs -> do
    let sceneMemChart = compileMemChart drvModMap tui ttable
        sceneMemChart' =
          sceneMemChart
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = ViewPort (0, vy0) (300, vy1)
            }
    renderScene vb sceneMemChart'
  -- timing range bar
  for_ (Map.lookup "timing-range" wcfg) $ \vpCvs -> do
    let sceneTimingRange = compileTimingRange tui ttable
        sceneTimingRange' =
          sceneTimingRange
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
            }
    renderScene vb sceneTimingRange'
  -- blocker lines
  let minfo = do
        hoveredMod <- tui ^. timingUIHoveredModule
        vpCvs <- Map.lookup "blockers" wcfg
        pure (hoveredMod, vpCvs)
  -- NOTE: vpCvs is ignored as dynamic size overrides it.
  -- TODO: clipping is still valid. we need two-layer viewport system.
  for_ minfo $ \(hoveredMod, _vpCvs) -> do
    let sceneBlockers = compileBlockers hoveredMod ttable
        ViewPort (vx0', vy0') (vx1', vy1') = sceneLocalViewPort sceneBlockers
        w = vx1' - vx0'
        h = vy1' - vy0'
        offsetY = timingHeight + timingRangeHeight
        sceneBlockers' =
          sceneBlockers
            { sceneGlobalViewPort = ViewPort (0, offsetY) (w, h + offsetY)
            }
    renderScene vb sceneBlockers'
