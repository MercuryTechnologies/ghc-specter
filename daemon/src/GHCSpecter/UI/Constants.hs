{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Constants (
  -- * time interval
  chanUpdateInterval,
  uiUpdateInterval,
  tickInterval,

  -- * Timing view
  timingMaxWidth,
  timingWidth,
  timingHeight,
  timingRangeHeight,

  -- * module graph
  modGraphWidth,
  modGraphHeight,

  -- * global
  canvasDim,

  -- * web
  widgetHeight,
) where

import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)

chanUpdateInterval :: NominalDiffTime
chanUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 10))

tickInterval :: NominalDiffTime
tickInterval = secondsToNominalDiffTime 1

timingMaxWidth :: (Num a) => a
timingMaxWidth = 10240

timingWidth :: (Num a) => a
timingWidth = 1440

timingHeight :: (Num a) => a
timingHeight = 600

timingRangeHeight :: (Num a) => a
timingRangeHeight = 10

modGraphWidth :: (Num a) => a
modGraphWidth = 1440

modGraphHeight :: (Num a) => a
modGraphHeight = 768

canvasDim :: (Num a) => (a, a)
canvasDim = (1440, 768)

-- TODO: this web-specific code should be located elsewhere.
widgetHeight :: Bool -> Text
widgetHeight isPaused
  | isPaused = "50vh"
  | otherwise = "95vh"
