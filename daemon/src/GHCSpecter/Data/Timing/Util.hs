{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Timing.Util
  ( -- * timing info utilities
    isTimeInTimerRange,
    isModuleCompilationDone,

    -- * calculate timing info and blocker graph
    makeTimingTable,
    makeBlockerGraph,
  )
where

import Control.Lens (makeClassy, to, (&), (.~), (^.), (^?), _2, _Just)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    diffUTCTime,
  )
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
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
import GHCSpecter.GraphLayout.Algorithm.Builder (makeBiDep)
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
  KeyMap DriverId Timer ->
  BiKeyMap DriverId ModuleName ->
  ModuleGraphInfo ->
  UTCTime ->
  TimingTable
makeTimingTable timing drvModMap mgi sessStart =
  emptyTimingTable
    & (ttableTimingInfos .~ timingInfos)
      . (ttableBlockingUpstreamDependency .~ lastDepMap)
      . (ttableBlockedDownstreamDependency .~ lastDepRevMap)
  where
    findModName drvId = forwardLookup drvId drvModMap
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

    -- Nothing case is stripped out in this var.
    timingInfos' =
      mapMaybe (\(mn, t) -> (,t) <$> mn) timingInfos

    modNameMap = mginfoModuleNameMap mgi
    -- TODO: This should be cached.
    nameModMap = M.fromList $ fmap swap $ IM.toList modNameMap
    modDep = mginfoModuleDep mgi
    findLastDep modu = do
      modId <- M.lookup modu nameModMap
      upIds <- IM.lookup modId modDep
      upNames <- traverse (`IM.lookup` modNameMap) upIds
      let isMyUpstream = (`elem` upNames) . fst
          upTiming = filter isMyUpstream timingInfos'
      if (null upTiming)
        then Nothing
        else pure $ L.maximumBy (compare `on` (^. _2 . timingEnd)) upTiming

    lastDepMap =
      M.fromList $
        mapMaybe (\(modu, _) -> (modu,) . fst <$> findLastDep modu) timingInfos'

    lastDepRevMap = M.foldlWithKey (\(!acc) d u -> M.alter (upd d) u acc) M.empty lastDepMap
      where
        upd d' Nothing = Just [d']
        upd d' (Just ds) = Just (d' : ds)

makeBlockerGraph ::
  ModuleGraphInfo ->
  TimingTable ->
  -- Map ModuleName ModuleName
  [(Int, Int)]
makeBlockerGraph mgi ttable = blockerGraph
  where
    upblocker = ttable ^. ttableBlockingUpstreamDependency
    downblocked = ttable ^. ttableBlockedDownstreamDependency
    blockers = M.keys $ M.filter (not . null) downblocked
    blockerGraph_ = M.toList $ M.filterWithKey (\k _ -> k `elem` blockers) upblocker
    modNameMap = mginfoModuleNameMap mgi
    -- TODO: This should be cached
    nameModMap = M.fromList $ fmap swap $ IM.toList modNameMap
    blockerGraph =
      mapMaybe (\(curr, upper) -> (,) <$> M.lookup curr nameModMap <*> M.lookup upper nameModMap) blockerGraph_
