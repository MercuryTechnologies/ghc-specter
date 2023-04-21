{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Components.TimingView (
  diffTime2X,
  module2Y,

  -- * compile renderer
  compileRules,
  compileTimingChart,
  compileMemChart,
  compileTimingRange,
  compileBlockers,
) where

import Control.Lens (to, (%~), (^.), _1, _2)
import Control.Monad (join)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Time.Clock (
  NominalDiffTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (MemInfo (..))
import GHCSpecter.Data.Map (
  BiKeyMap,
  forwardLookup,
 )
import GHCSpecter.Data.Timing.Types (
  HasPipelineInfo (..),
  HasTimingTable (..),
  TimingTable,
 )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Primitive (..),
  Scene (..),
  TextFontFace (..),
  TextPosition (..),
  ViewPort (..),
 )
import GHCSpecter.UI.Constants (
  timingHeight,
  timingMaxWidth,
  timingRangeHeight,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  HasViewPortInfo (..),
  TimingUI,
 )
import GHCSpecter.UI.Types.Event (
  TimingEvent (..),
 )
import Prelude hiding (div)

diffTime2X :: NominalDiffTime -> NominalDiffTime -> Double
diffTime2X totalTime time =
  realToFrac (time / totalTime) * timingMaxWidth

module2Y :: Int -> Double
module2Y i = 5.0 * fromIntegral i + 1.0

isInRange :: (Int, a) -> (Double, Double) -> Bool
(i, _) `isInRange` (y0, y1) =
  let y = module2Y i
   in y0 <= y && y <= y1

colorCodes :: [Color]
colorCodes =
  [ ColorRedLevel5
  , ColorRedLevel4
  , ColorRedLevel3
  , ColorRedLevel2
  , ColorRedLevel1
  , ColorRedLevel0
  ]

compileRules ::
  Bool ->
  TimingTable ->
  Int ->
  NominalDiffTime ->
  [Primitive e]
compileRules showParallel table totalHeight totalTime =
  ( if showParallel
      then fmap box rangesWithCPUUsage
      else []
  )
    ++ fmap line ruleTimes
  where
    totalTimeInSec = nominalDiffTimeToSeconds totalTime
    ruleTimes = [0, 1 .. totalTimeInSec]
    ranges = zip ruleTimes (tail ruleTimes)
    getParallelCompilation (sec1, sec2) =
      let avg = secondsToNominalDiffTime $ realToFrac $ 0.5 * (sec1 + sec2)
          filtered =
            filter
              (\x -> x ^. _2 . to (isTimeInTimerRange avg))
              (table ^. ttableTimingInfos)
       in length filtered
    rangesWithCPUUsage =
      fmap (\range -> (range, getParallelCompilation range)) ranges
    nCPU2Color n
      | n <= 2 = colorCodes !! 0
      | n > 2 && n <= 4 = colorCodes !! 1
      | n > 4 && n <= 6 = colorCodes !! 2
      | n > 6 && n <= 8 = colorCodes !! 3
      | n > 8 && n <= 10 = colorCodes !! 4
      | otherwise = colorCodes !! 5
    line sec =
      let x = sec2X sec
       in Polyline (x, 0) [] (x, fromIntegral totalHeight) Gray 0.25
    sec2X sec =
      diffTime2X totalTime (secondsToNominalDiffTime sec)
    box ((sec1, _), n) =
      Rectangle
        (sec2X sec1, 0)
        (sec2X (1.01))
        (fromIntegral totalHeight)
        Nothing
        (Just (nCPU2Color n))
        Nothing
        Nothing

compileTimingChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Scene TimingEvent
compileTimingChart drvModMap tui ttable =
  Scene
    { sceneId = "timing-chart"
    , sceneGlobalViewPort = ViewPort (0, 0) (timingWidth, timingHeight)
    , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingHeight)
    , sceneElements =
        compileRules (tui ^. timingUIHowParallel) ttable totalHeight totalTime
          ++ (concatMap makeItem filteredItems)
          ++ lineToUpstream
          ++ linesToDownstream
    }
  where
    timingInfos = ttable ^. ttableTimingInfos
    mhoveredMod = tui ^. timingUIHoveredModule
    nMods = length timingInfos
    modEndTimes = fmap (^. _2 . plEnd . _1) timingInfos
    totalTime =
      case modEndTimes of
        [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
        _ -> maximum modEndTimes
    totalHeight = 5 * nMods
    leftOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
       in diffTime2X totalTime startTime
    rightOfBox (_, tinfo) =
      let endTime = tinfo ^. plEnd . _1
       in diffTime2X totalTime endTime
    widthOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          endTime = tinfo ^. plEnd . _1
       in diffTime2X totalTime (endTime - startTime)
    widthHscOutOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          hscOutTime = tinfo ^. plHscOut . _1
       in diffTime2X totalTime (hscOutTime - startTime)
    widthAsOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          asTime = tinfo ^. plAs . _1
       in diffTime2X totalTime (asTime - startTime)

    box (i, item@(mmodu, _)) =
      let highlighter
            | mmodu == mhoveredMod = (Just Orange, Just 0.5)
            | otherwise = (Nothing, Nothing)
          mhitEvent = do
            modu <- mmodu
            pure
              HitEvent
                { hitEventHoverOn = Just (HoverOnModule modu)
                , hitEventHoverOff = Just (HoverOffModule modu)
                , hitEventClick = Nothing
                }
       in Rectangle
            (leftOfBox item, module2Y i)
            (widthOfBox item)
            3
            (highlighter ^. _1)
            (Just LightSlateGray)
            (highlighter ^. _2)
            mhitEvent
    boxHscOut (i, item) =
      Rectangle
        (leftOfBox item, module2Y i)
        (widthHscOutOfBox item)
        3
        Nothing
        (Just RoyalBlue)
        Nothing
        Nothing
    boxAs (i, item) =
      Rectangle
        (leftOfBox item, module2Y i)
        (widthAsOfBox item)
        3
        Nothing
        (Just DeepSkyBlue)
        Nothing
        Nothing
    moduleText (i, item@(mmodu, _)) =
      let fontSize = 4
          moduTxt = fromMaybe "" mmodu
       in DrawText (rightOfBox item, module2Y i + 3) LowerLeft Sans Black fontSize moduTxt
    makeItem x =
      if tui ^. timingUIPartition
        then [box x, boxAs x, boxHscOut x, moduleText x]
        else [box x, moduleText x]
    timingInfos' = fmap (_1 %~ (`forwardLookup` drvModMap)) timingInfos
    allItems = zip [0 ..] timingInfos'
    rangeY =
      let vpi = tui ^. timingUIViewPort
          vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
       in (topLeft vp ^. _2, bottomRight vp ^. _2)
    filteredItems = filter (`isInRange` rangeY) allItems
    mkLine src tgt = do
      (srcIdx, srcItem) <-
        L.find (\(_, (mname, _)) -> mname == Just src) allItems
      (tgtIdx, tgtItem) <-
        L.find (\(_, (mname, _)) -> mname == Just tgt) allItems
      let line =
            Polyline
              (leftOfBox srcItem, module2Y srcIdx)
              []
              (rightOfBox tgtItem, module2Y tgtIdx)
              Red
              1.0
      pure line
    lineToUpstream = maybeToList $ join $ fmap mkLineToUpstream mhoveredMod
      where
        mkLineToUpstream hoveredMod = do
          upMod <-
            M.lookup hoveredMod (ttable ^. ttableBlockingUpstreamDependency)
          mkLine hoveredMod upMod
    linesToDownstream = maybe [] (fromMaybe [] . mkLinesToDownstream) mhoveredMod
      where
        mkLinesToDownstream :: ModuleName -> Maybe [Primitive TimingEvent]
        mkLinesToDownstream hoveredMod = do
          downMods <-
            M.lookup hoveredMod (ttable ^. ttableBlockedDownstreamDependency)
          pure $ mapMaybe (`mkLine` hoveredMod) downMods

compileMemChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Scene e
compileMemChart drvModMap tui ttable =
  Scene
    { sceneId = "mem-chart"
    , sceneGlobalViewPort = ViewPort (0, 0) (300, timingHeight)
    , sceneLocalViewPort = ViewPort (0, 0) (300, timingHeight)
    , sceneElements =
        concatMap makeItem filteredItems
    }
  where
    timingInfos = ttable ^. ttableTimingInfos
    timingInfos' = fmap (_1 %~ (`forwardLookup` drvModMap)) timingInfos
    allItems = zip [0 ..] timingInfos'
    rangeY =
      let vpi = tui ^. timingUIViewPort
          vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
       in (topLeft vp ^. _2, bottomRight vp ^. _2)
    filteredItems = filter (`isInRange` rangeY) allItems
    alloc2X alloc =
      let
        -- ratio to 16 GiB
        allocRatio :: Double
        allocRatio = fromIntegral alloc / (16 * 1024 * 1024 * 1024)
       in
        allocRatio * 150
    widthOfBox minfo = alloc2X (negate (memAllocCounter minfo))
    box color lz (i, item) =
      case item ^. _2 . lz . _2 of
        Nothing -> []
        Just minfo ->
          [ Rectangle
              (0, module2Y i)
              (widthOfBox minfo)
              3
              Nothing
              (Just color)
              Nothing
              Nothing
          ]
    moduleText (i, (mmodu, _)) =
      let fontSize = 4
          moduTxt = fromMaybe "" mmodu
       in DrawText (150, module2Y i + 3) LowerLeft Sans Black fontSize moduTxt
    makeItem x =
      if (tui ^. timingUIPartition)
        then
          box LightSlateGray plEnd x
            ++ box DeepSkyBlue plAs x
            ++ box RoyalBlue plHscOut x
            ++ [moduleText x]
        else
          box LightSlateGray plEnd x
            ++ [moduleText x]

compileTimingRange ::
  TimingUI ->
  TimingTable ->
  Scene e
compileTimingRange tui ttable =
  Scene
    { sceneId = "timing-range"
    , sceneGlobalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
    , sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight)
    , sceneElements = [background, handle]
    }
  where
    timingInfos = ttable ^. ttableTimingInfos
    nMods = length timingInfos
    allItems = zip [0 ..] timingInfos
    rangeY =
      let vpi = tui ^. timingUIViewPort
          vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
       in (topLeft vp ^. _2, bottomRight vp ^. _2)
    filteredItems = filter (`isInRange` rangeY) allItems

    (minI, maxI) =
      let idxs = fmap (^. _1) filteredItems
       in (minimum idxs, maximum idxs)

    convert i = floor @Double (fromIntegral i / fromIntegral nMods * timingWidth)
    handleX :: Int
    handleX = if null filteredItems then 0 else convert minI
    handleWidth :: Int
    handleWidth = if null filteredItems then 0 else convert (maxI - minI + 1)

    background =
      Rectangle
        (0, 0)
        timingWidth
        timingRangeHeight
        Nothing
        (Just LightGray)
        Nothing
        Nothing
    handle =
      Rectangle
        (fromIntegral handleX, 0)
        (fromIntegral handleWidth)
        timingRangeHeight
        (Just Black)
        (Just White)
        (Just 1.0)
        Nothing

compileBlockers :: ModuleName -> TimingTable -> Scene e
compileBlockers hoveredMod ttable =
  Scene
    { sceneId = "blockers"
    , sceneGlobalViewPort = ViewPort (0, 0) (200, size)
    , sceneLocalViewPort = ViewPort (0, 0) (200, size)
    , sceneElements = box : contents
    }
  where
    upMods =
      maybeToList (M.lookup hoveredMod (ttable ^. ttableBlockingUpstreamDependency))
    downMods =
      fromMaybe [] (M.lookup hoveredMod (ttable ^. ttableBlockedDownstreamDependency))
    --
    selected = DrawText (0, 0) UpperLeft Sans Black 8 hoveredMod
    line = Polyline (0, 0) [] (200, 0) Black 1
    blockedBy = DrawText (0, 0) UpperLeft Sans Black 8 "Blocked By"
    upstreams = fmap (DrawText (0, 0) UpperLeft Sans Black 8) upMods
    blocking = DrawText (0, 0) UpperLeft Sans Black 8 "Blocking"
    downstreams = fmap (DrawText (0, 0) UpperLeft Sans Black 8) downMods
    --
    placing !offset item =
      case item of
        DrawText (x, y) p' ff c fs t ->
          let doffset = fromIntegral fs + 4
           in (offset + doffset, DrawText (x, y + offset) p' ff c fs t)
        Polyline (x0, y0) [] (x1, y1) c w ->
          let doffset = 5
           in (offset + doffset, Polyline (x0, y0 + offset + 3) [] (x1, y1 + offset + 3) c w)
        -- for now
        _ -> (offset, item)
    --
    (size, contents) =
      L.mapAccumL placing 0 ([selected, line, blockedBy] ++ upstreams ++ [line, blocking] ++ downstreams)
    box = Rectangle (0, 0) 200 size (Just Black) Nothing (Just 1.0) Nothing
