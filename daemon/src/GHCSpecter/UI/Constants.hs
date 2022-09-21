module GHCSpecter.UI.Constants
  ( chanUpdateInterval,
    uiUpdateInterval,
    tickInterval,
    timingWidth,
    timingHeight,
  )
where

import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)

chanUpdateInterval :: NominalDiffTime
chanUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 10))

tickInterval :: NominalDiffTime
tickInterval = secondsToNominalDiffTime 1

timingWidth :: Int
timingWidth = 800

timingHeight :: Int
timingHeight = 600
