{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Timing.Util
  ( -- * timing info utilities
    isTimeInTimerRange,
    isModuleCompilationDone,

    -- * construct timing info table
    makeTimingTable,
  )
where

import Control.Lens (makeClassy, to, (&), (.~), (^.), (^?), _2, _Just)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.Maybe (isJust, mapMaybe)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
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
import GHCSpecter.Data.Timing.Types
  ( HasTimingInfo (..),
    HasTimingTable (..),
    TimingInfo (..),
    TimingTable,
    emptyTimingTable,
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

isTimeInTimerRange :: (Ord a) => a -> TimingInfo a -> Bool
isTimeInTimerRange x tinfo =
  x >= (tinfo ^. timingStart) && x <= (tinfo ^. timingEnd)

isModuleCompilationDone :: BiKeyMap DriverId ModuleName -> KeyMap DriverId Timer -> ModuleName -> Bool
isModuleCompilationDone drvModMap timing modu =
  isJust $ do
    i <- backwardLookup modu drvModMap
    timing ^? to (lookupKey i) . _Just . to getEndTime . _Just

makeTimingTable ::
  -- ServerState -> TimingTable
  KeyMap DriverId Timer ->
  BiKeyMap DriverId ModuleName ->
  UTCTime ->
  TimingTable
makeTimingTable timing drvModMap sessStart =
  let findModName drvId = forwardLookup drvId drvModMap

      subtractTime (modName, timer) = do
        modStartTime <- getStartTime timer
        modHscOutTime <- getHscOutTime timer
        modAsTime <- getAsTime timer
        modEndTime <- getEndTime timer
        let modStartTimeDiff = modStartTime `diffUTCTime` sessStart
            modHscOutTimeDiff = modHscOutTime `diffUTCTime` sessStart
            modAsTimeDiff = modAsTime `diffUTCTime` sessStart
            modEndTimeDiff = modEndTime `diffUTCTime` sessStart
            tinfo =
              TimingInfo
                { _timingStart = modStartTimeDiff
                , _timingHscOut = modHscOutTimeDiff
                , _timingAs = modAsTimeDiff
                , _timingEnd = modEndTimeDiff
                }
        pure (modName, tinfo)
      timingInfos =
        fmap (first findModName)
          . L.sortOn (^. _2 . timingStart)
          . mapMaybe subtractTime
          $ keyMapToList timing
   in emptyTimingTable & (ttableTimingInfos .~ timingInfos)
