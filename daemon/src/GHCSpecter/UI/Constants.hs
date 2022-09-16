module GHCSpecter.UI.Constants
  ( chanUpdateInterval,
    uiUpdateInterval,
  )
where

import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)

chanUpdateInterval :: NominalDiffTime
chanUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = secondsToNominalDiffTime (fromRational (1 / 10))
