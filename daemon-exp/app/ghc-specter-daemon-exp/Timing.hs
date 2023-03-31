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
  compileRules,
  compileTimingBar,
  compileTimingChart,
  diffTime2X,
  module2Y,
  viewPortX,
  viewPortY,
 )
import GHCSpecter.UI.Constants (
  timingBarHeight,
  timingHeight,
  timingMaxWidth,
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
      rexpTimingBar :: [Primitive]
      rexpTimingBar = compileTimingBar tui ttable
  R.save
  R.rectangle 0 0 timingWidth timingHeight
  R.clip
  traverse_ (renderPrimitive vb) rexpTimingChart
  R.restore
  R.save
  R.translate 0 timingHeight
  traverse_ (renderPrimitive vb) rexpTimingBar
  R.restore
