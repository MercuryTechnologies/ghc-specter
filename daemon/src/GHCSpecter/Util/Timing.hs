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

import Control.Lens (at, makeClassy, to, (^.), (^?), _2, _Just)
import Data.Bifunctor (first)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
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

isModuleCompilationDone :: Map ModuleName DriverId -> IntMap Timer -> ModuleName -> Bool
isModuleCompilationDone modDrvMap timing modu =
  isJust $ do
    DriverId i <- M.lookup modu modDrvMap
    timing ^? at i . _Just . to getEndTime . _Just

makeTimingTable :: ServerState -> [(Maybe ModuleName, TimingInfo NominalDiffTime)]
makeTimingTable ss =
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> []
    Just sessionStartTime ->
      let timing = ss ^. serverTiming
          drvModMap = ss ^. serverDriverModuleMap
          findModName i = IM.lookup i drvModMap

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
            $ IM.toList timing
