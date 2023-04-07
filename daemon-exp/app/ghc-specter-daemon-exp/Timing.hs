{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Lens ((^.))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Data.Map (BiKeyMap)
import GHCSpecter.Data.Timing.Types (TimingTable)
import GHCSpecter.Graphics.DSL (Primitive (..))
import GHCSpecter.Render.Components.TimingView (
  compileMemChart,
  compileTimingChart,
  compileTimingRange,
 )
import GHCSpecter.UI.Constants (
  timingHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasViewPortInfo (..),
  TimingUI,
  ViewPort (..),
 )
import GI.Cairo.Render qualified as R
import Renderer (renderPrimitive)
import Types (ViewBackend)

renderTiming ::
  ViewBackend ->
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  R.Render ()
renderTiming vb drvModMap tui ttable = do
  let rexpTimingChart :: [Primitive]
      rexpTimingChart = compileTimingChart drvModMap tui ttable
      rexpMemChart :: [Primitive]
      rexpMemChart = compileMemChart drvModMap tui ttable
      rexpTimingBar :: [Primitive]
      rexpTimingBar = compileTimingRange tui ttable
  -- timing chart
  R.save
  R.rectangle 0 0 (timingWidth * 0.8) timingHeight
  R.clip
  -- TODO: refactor this out
  let vpi = tui ^. timingUIViewPort
      ViewPort (vx0, vy0) (vx1, vy1) = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
      scaleX = timingWidth / (vx1 - vx0)
      scaleY = timingHeight / (vy1 - vy0)
  R.scale scaleX scaleY
  R.translate (-vx0) (-vy0)
  traverse_ (renderPrimitive vb) rexpTimingChart
  R.restore
  -- mem chart
  R.save
  R.rectangle (timingWidth * 0.8) 0 timingWidth timingHeight
  R.clip
  R.translate (timingWidth * 0.8) 0
  R.scale 1.0 scaleY
  R.translate 0 (-vy0)
  traverse_ (renderPrimitive vb) rexpMemChart
  R.restore
  -- timing range
  R.save
  R.translate 0 timingHeight
  traverse_ (renderPrimitive vb) rexpTimingBar
  R.restore
