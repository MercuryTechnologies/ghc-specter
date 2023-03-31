{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Lens (to, (%~), (^.), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (
  NominalDiffTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Data.Map (
  BiKeyMap,
  forwardLookup,
 )
import GHCSpecter.Data.Timing.Types (
  HasPipelineInfo (..),
  HasTimingTable (..),
  TimingTable,
 )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..), TextPosition (..))
import GHCSpecter.Render.Components.TimingView (
  compileMemChart,
  compileTimingChart,
  compileTimingRange,
  diffTime2X,
  module2Y,
  viewPortX,
  viewPortY,
 )
import GHCSpecter.UI.Constants (
  timingHeight,
  timingMaxWidth,
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  TimingUI (..),
 )
import GI.Cairo.Render qualified as R
import Types (ViewBackend)
import Util (renderPrimitive)

renderTiming ::
  ViewBackend ->
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  R.Render ()
renderTiming vb drvModMap tui ttable = do
  let timingInfos = ttable ^. ttableTimingInfos
      nMods = length timingInfos
      modEndTimes = fmap (^. _2 . plEnd . _1) timingInfos
      totalHeight = 5 * nMods
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      rexpTimingChart :: [Primitive]
      rexpTimingChart = compileTimingChart drvModMap tui ttable
      rexpMemChart :: [Primitive]
      rexpMemChart = compileMemChart drvModMap tui ttable
      rexpTimingBar :: [Primitive]
      rexpTimingBar = compileTimingRange tui ttable
  -- timing chart
  R.save
  R.rectangle 0 0 (timingWidth * 0.8) timingHeight
  R.clip
  traverse_ (renderPrimitive vb) rexpTimingChart
  R.restore
  -- mem chart
  R.save
  R.translate (timingWidth * 0.8) 0
  traverse_ (renderPrimitive vb) rexpMemChart
  R.restore
  -- timing range
  R.save
  R.translate 0 timingHeight
  traverse_ (renderPrimitive vb) rexpTimingBar
  R.restore
