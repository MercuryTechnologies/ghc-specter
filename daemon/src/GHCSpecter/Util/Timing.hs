{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Util.Timing
  ( -- * extracted timing information
    TimingInfo (..),
    HasTimingInfo (..),

    -- * timing info utilities
    isTimeInTimerRange,
    isModuleCompilationDone,

    -- * construct timing info table
    makeTimingTable,
  )
where

import Control.Lens (makeClassy, to, (^.), (^?), _2, _Just)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Maybe (isJust, mapMaybe)
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
  )
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( SessionInfo (..),
    Timer (..),
    getAsTime,
    getEndTime,
    getHscOutTime,
    getStartTime,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )
import GHCSpecter.Util.Map
  ( BiKeyMap,
    KeyMap,
    backwardLookup,
    forwardLookup,
    keyMapToList,
    lookupKey,
  )

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingAs :: a
  , _timingEnd :: a
  }
  deriving (Show)

makeClassy ''TimingInfo

isTimeInTimerRange :: (Ord a) => a -> TimingInfo a -> Bool
isTimeInTimerRange x tinfo =
  x >= (tinfo ^. timingStart) && x <= (tinfo ^. timingEnd)

isModuleCompilationDone :: BiKeyMap DriverId ModuleName -> KeyMap DriverId Timer -> ModuleName -> Bool
isModuleCompilationDone drvModMap timing modu =
  isJust $ do
    i <- backwardLookup modu drvModMap
    timing ^? to (lookupKey i) . _Just . to getEndTime . _Just

makeTimingTable :: ServerState -> [(Maybe ModuleName, TimingInfo NominalDiffTime)]
makeTimingTable ss =
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> []
    Just sessionStartTime ->
      let timing = ss ^. serverTiming
          drvModMap = ss ^. serverDriverModuleMap
          findModName drvId = forwardLookup drvId drvModMap

          subtractTime (modName, timer) = do
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
       in fmap (first findModName)
            . L.sortOn (^. _2 . timingStart)
            . mapMaybe subtractTime
            $ keyMapToList timing
