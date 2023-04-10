{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((.~), (^.))
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Data.Map (BiKeyMap)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (
  Primitive (..),
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
  HasUIState (..),
  HasUIViewRaw (..),
  HasViewPortInfo (..),
  TimingUI,
  UIState,
 )
import GI.Cairo.Render qualified as R
import Renderer (
  renderPrimitive,
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
  R.liftIO $ atomically $ modifyTVar' uiRef (uiViewRaw . uiRawEventMap .~ [])
  let vpi = tui ^. timingUIViewPort
      vp@(ViewPort (vx0, vy0) (vx1, vy1)) = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
      sceneTimingChart = compileTimingChart drvModMap tui ttable
      sceneMemChart = compileMemChart drvModMap tui ttable
      sceneTimingRange = compileTimingRange tui ttable
  -- timing chart
  let sceneTimingChart' =
        sceneTimingChart
          { sceneGlobalViewPort = ViewPort (0, 0) (timingWidth * 0.8, timingHeight)
          , sceneLocalViewPort = vp
          }
  renderScene uiRef vb sceneTimingChart'
  -- mem chart
  let ViewPort (cx0, cy0) (cx1, cy1) = sceneGlobalViewPort sceneTimingChart'
      ViewPort (vx0, vy0) (vx1, vy1) = sceneLocalViewPort sceneTimingChart'
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
      sceneMemChart' =
        sceneMemChart
          { sceneGlobalViewPort = ViewPort (timingWidth * 0.8, 0) (timingWidth, timingHeight)
          , sceneLocalViewPort = ViewPort (0, vy0) (300, vy1)
          }
  renderScene uiRef vb sceneMemChart'
  -- timing range bar
  let sceneTimingRange' =
        sceneTimingRange
          { sceneGlobalViewPort = ViewPort (0, timingHeight) (timingWidth, timingHeight + timingRangeHeight)
          , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
          }
  renderScene uiRef vb sceneTimingRange'
  -- blocker lines
  for_ (tui ^. timingUIHoveredModule) $ \hoveredMod -> do
    let sceneBlockers = compileBlockers hoveredMod ttable
        ViewPort (vx0, vy0) (vx1, vy1) = sceneLocalViewPort sceneBlockers
        w = vx1 - vx0
        h = vy1 - vy0
        offsetY = timingHeight + timingRangeHeight
        sceneBlockers' =
          sceneBlockers
            { sceneGlobalViewPort = ViewPort (0, offsetY) (w, h + offsetY)
            }
    renderScene uiRef vb sceneBlockers'
