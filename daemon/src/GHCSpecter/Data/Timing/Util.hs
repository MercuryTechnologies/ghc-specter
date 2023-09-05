{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe)
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
  )
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    Timer (..),
    getAs,
    getEnd,
    getHscOut,
    getStart,
  )
import GHCSpecter.Data.Map
  ( BiKeyMap,
    KeyMap,
    backwardLookup,
    forwardLookup,
    keyMapToList,
    lookupKey,
  )
import GHCSpecter.Data.Timing.Types
  ( PipelineInfo (..),
    TimingTable (..),
    emptyTimingTable,
  )

isTimeInTimerRange :: (Ord a) => a -> PipelineInfo (a, b) -> Bool
isTimeInTimerRange x tinfo =
  x >= (fst tinfo._plStart) && x <= (fst tinfo._plEnd)

isModuleCompilationDone :: BiKeyMap DriverId ModuleName -> KeyMap DriverId Timer -> ModuleName -> Bool
isModuleCompilationDone drvModMap timing modu =
  isJust $ do
    i <- backwardLookup modu drvModMap
    (fmap fst . getEnd =<< lookupKey i timing)

makeTimingTable ::
  KeyMap DriverId Timer ->
  BiKeyMap DriverId ModuleName ->
  ModuleGraphInfo ->
  UTCTime ->
  TimingTable
makeTimingTable timing drvModMap mgi sessStart =
  emptyTimingTable
    { _ttableTimingInfos = timingInfos,
      _ttableBlockingUpstreamDependency = lastDepMap,
      _ttableBlockedDownstreamDependency = lastDepRevMap
    }
  where
    findModName drvId = forwardLookup drvId drvModMap
    subtractTime (modName, timer) = do
      (modStartTime, mmodStartMem) <- getStart timer
      (modHscOutTime, mmodHscOutMem) <- getHscOut timer
      (modAsTime, mmodAsMem) <- getAs timer
      (modEndTime, mmodEndMem) <- getEnd timer
      let modStartTimeDiff = modStartTime `diffUTCTime` sessStart
          modHscOutTimeDiff = modHscOutTime `diffUTCTime` sessStart
          modAsTimeDiff = modAsTime `diffUTCTime` sessStart
          modEndTimeDiff = modEndTime `diffUTCTime` sessStart
          tinfo =
            PipelineInfo
              { _plStart = (modStartTimeDiff, mmodStartMem),
                _plHscOut = (modHscOutTimeDiff, mmodHscOutMem),
                _plAs = (modAsTimeDiff, mmodAsMem),
                _plEnd = (modEndTimeDiff, mmodEndMem)
              }
      pure (modName, tinfo)
    timingInfos =
      L.sortOn (fst . _plStart . snd)
        . mapMaybe subtractTime
        $ keyMapToList timing

    -- Nothing case is stripped out in this var.
    timingInfos' =
      mapMaybe (\(i, t) -> findModName i >>= \n -> pure (n, t)) timingInfos

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
        else pure $ L.maximumBy (compare `on` (fst . _plEnd . snd)) upTiming

    lastDepMap =
      M.fromList $
        mapMaybe (\(modu, _) -> (modu,) . fst <$> findLastDep modu) timingInfos'

    lastDepRevMap = M.foldlWithKey (\(!acc) d u -> M.alter (upd d) u acc) M.empty lastDepMap
      where
        upd d' Nothing = Just [d']
        upd d' (Just ds) = Just (d' : ds)

makeBlockerGraph ::
  Int ->
  ModuleGraphInfo ->
  TimingTable ->
  IntMap [Int]
makeBlockerGraph blockerThreshold mgi ttable = blockerGraph
  where
    modNameMap = mginfoModuleNameMap mgi
    -- TODO: This should be cached
    nameModMap = M.fromList $ fmap swap $ IM.toList modNameMap
    modDep = mginfoModuleDep mgi

    blocked = ttable._ttableBlockedDownstreamDependency
    blockers =
      mapMaybe (\k -> M.lookup k nameModMap) $
        M.keys $
          M.filter (\ds -> length ds >= blockerThreshold) blocked
    blockerGraph = fmap (filter (`elem` blockers)) $ IM.filterWithKey (\k _ -> k `elem` blockers) modDep
