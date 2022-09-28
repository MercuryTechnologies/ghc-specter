{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Util.Timing
  ( -- * extracted timing information
    TimingInfo (..),
    HasTimingInfo (..),

    -- * timing info utilities
    isInProgress,

    -- * construct timing info table
    makeTimingTable,
  )
where

import Control.Lens (makeClassy, to, (^.), _2)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
  )
import GHCSpecter.Channel.Common.Types (type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( SessionInfo (..),
    getAsTime,
    getEndTime,
    getHscOutTime,
    getStartTime,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingAs :: a
  , _timingEnd :: a
  }
  deriving (Show)

makeClassy ''TimingInfo

isInProgress :: (Ord a) => a -> TimingInfo a -> Bool
isInProgress x tinfo =
  x >= (tinfo ^. timingStart) && x <= (tinfo ^. timingEnd)

makeTimingTable :: ServerState -> [(ModuleName, TimingInfo NominalDiffTime)]
makeTimingTable ss =
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> []
    Just sessionStartTime ->
      let subtractTime (modName, timer) = do
            modStartTime <- getStartTime timer
            modHscOutTime <- getHscOutTime timer
            modAsTime <- getAsTime timer
            modEndTime <- getEndTime timer
            let modStartTimeDiff = modStartTime `diffUTCTime` sessionStartTime
                modHscOutTimeDiff = modHscOutTime `diffUTCTime` sessionStartTime
                modAsTimeDiff = modAsTime `diffUTCTime` sessionStartTime
                modEndTimeDiff = modEndTime `diffUTCTime` sessionStartTime
                tinfo =
                  TimingInfo
                    { _timingStart = modStartTimeDiff
                    , _timingHscOut = modHscOutTimeDiff
                    , _timingAs = modAsTimeDiff
                    , _timingEnd = modEndTimeDiff
                    }
            pure (modName, tinfo)
       in L.sortOn (^. _2 . timingStart) $ mapMaybe subtractTime $ M.toList $ ss ^. serverTiming
