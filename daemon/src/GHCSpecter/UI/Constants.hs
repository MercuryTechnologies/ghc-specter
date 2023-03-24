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
  timingBarHeight,
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

timingWidth :: Int
timingWidth = 1600

timingHeight :: Int
timingHeight = 600

timingBarHeight :: Int
timingBarHeight = 10

widgetHeight :: Bool -> Text
widgetHeight isPaused
  | isPaused = "50vh"
  | otherwise = "95vh"
