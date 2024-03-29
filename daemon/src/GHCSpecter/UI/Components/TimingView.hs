{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Components.TimingView
  ( diffTime2X,
    module2Y,

    -- * build renderer
    buildRules,
    buildTimingChart,
    buildMemChart,
    buildTimingRange,
    buildBlockers,
  )
where

import Control.Monad (join)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup (sconcat)
import Data.Time.Clock
  ( NominalDiffTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (MemInfo (..))
import GHCSpecter.Data.Map
  ( BiKeyMap,
    forwardLookup,
  )
import GHCSpecter.Data.Timing.Types
  ( PipelineInfo (..),
    TimingTable (..),
  )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Graphics.DSL
  ( Color (..),
    HitEvent (..),
    Primitive (..),
    Scene (..),
    TextFontFace (..),
    TextPosition (..),
    ViewPort (..),
    polyline,
    rectangle,
    viewPortHeight,
  )
import GHCSpecter.Layouter.Packer
  ( flowLineByLine,
    toSizedLine,
  )
import GHCSpecter.Layouter.Text
  ( MonadTextLayout,
    drawText',
  )
import GHCSpecter.UI.Constants
  ( timingHeight,
    timingMaxWidth,
    timingRangeHeight,
    timingWidth,
  )
import GHCSpecter.UI.Types
  ( TimingUI (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event
  ( TimingEvent (..),
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
  [ ColorRedLevel5,
    ColorRedLevel4,
    ColorRedLevel3,
    ColorRedLevel2,
    ColorRedLevel1,
    ColorRedLevel0
  ]

buildRules ::
  Bool ->
  TimingTable ->
  Int ->
  NominalDiffTime ->
  [Primitive e]
buildRules showParallel table totalHeight totalTime =
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
              (\x -> isTimeInTimerRange avg (snd x))
              (table._ttableTimingInfos)
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
       in polyline (x, 0) [] (x, fromIntegral totalHeight) Gray 0.25
    sec2X sec =
      diffTime2X totalTime (secondsToNominalDiffTime sec)
    box ((sec1, _), n) =
      rectangle
        (sec2X sec1, 0)
        (sec2X (1.01))
        (fromIntegral totalHeight)
        Nothing
        (Just (nCPU2Color n))
        Nothing
        Nothing

buildTimingChart ::
  forall m.
  (MonadTextLayout m) =>
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  m (Scene (Primitive TimingEvent))
buildTimingChart drvModMap tui ttable = do
  renderedItems <- concat <$> traverse makeItem filteredItems
  pure
    Scene
      { sceneId = "timing-chart",
        sceneGlobalViewPort = extent,
        sceneLocalViewPort = extent,
        sceneElements =
          buildRules (tui._timingUIHowParallel) ttable totalHeight totalTime
            ++ renderedItems
            ++ lineToUpstream
            ++ linesToDownstream,
        sceneExtents = Just extent
      }
  where
    extent = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)
    timingInfos = ttable._ttableTimingInfos
    mhoveredMod = tui._timingUIHoveredModule
    nMods = length timingInfos
    modEndTimes = fmap (fst . _plEnd . snd) timingInfos
    totalTime =
      case modEndTimes of
        [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
        _ -> maximum modEndTimes
    totalHeight = 5 * nMods
    leftOfBox (_, tinfo) =
      let startTime = fst tinfo._plStart
       in diffTime2X totalTime startTime
    rightOfBox (_, tinfo) =
      let endTime = fst tinfo._plEnd
       in diffTime2X totalTime endTime
    widthOfBox (_, tinfo) =
      let startTime = fst tinfo._plStart
          endTime = fst tinfo._plEnd
       in diffTime2X totalTime (endTime - startTime)
    widthHscOutOfBox (_, tinfo) =
      let startTime = fst tinfo._plStart
          hscOutTime = fst tinfo._plHscOut
       in diffTime2X totalTime (hscOutTime - startTime)
    widthAsOfBox (_, tinfo) =
      let startTime = fst tinfo._plStart
          asTime = fst tinfo._plAs
       in diffTime2X totalTime (asTime - startTime)

    box (i, item@(mmodu, _)) =
      let highlighter
            | mmodu == mhoveredMod = (Just Orange, Just 0.5)
            | otherwise = (Nothing, Nothing)
          mhitEvent = do
            modu <- mmodu
            pure
              HitEvent
                { hitEventHoverOn = Just (HoverOnModule modu),
                  hitEventHoverOff = Just (HoverOffModule modu),
                  hitEventClick = Nothing
                }
       in rectangle
            (leftOfBox item, module2Y i)
            (widthOfBox item)
            3
            (fst highlighter)
            (Just LightSlateGray)
            (snd highlighter)
            mhitEvent
    boxHscOut (i, item) =
      rectangle
        (leftOfBox item, module2Y i)
        (widthHscOutOfBox item)
        3
        Nothing
        (Just RoyalBlue)
        Nothing
        Nothing
    boxAs (i, item) =
      rectangle
        (leftOfBox item, module2Y i)
        (widthAsOfBox item)
        3
        Nothing
        (Just DeepSkyBlue)
        Nothing
        Nothing
    moduleText (i, item@(mmodu, _)) = do
      let fontSize = 4
          moduTxt = fromMaybe "" mmodu
      drawText' (rightOfBox item, module2Y i + 3) LowerLeft Sans Black fontSize moduTxt
    makeItem x = do
      renderedText <- moduleText x
      if tui._timingUIPartition
        then do
          pure [box x, boxAs x, boxHscOut x, renderedText]
        else pure [box x, renderedText]
    timingInfos' =
      fmap
        (\(drv_id, pinfo) -> (forwardLookup drv_id drvModMap, pinfo))
        timingInfos
    allItems = zip [0 ..] timingInfos'
    rangeY =
      let vpi = tui._timingUIViewPort
          vp = fromMaybe (vpi._vpViewPort) (vpi._vpTempViewPort)
       in (snd vp.topLeft, snd vp.bottomRight)
    filteredItems = filter (`isInRange` rangeY) allItems
    mkLine src tgt = do
      (srcIdx, srcItem) <-
        L.find (\(_, (mname, _)) -> mname == Just src) allItems
      (tgtIdx, tgtItem) <-
        L.find (\(_, (mname, _)) -> mname == Just tgt) allItems
      let line =
            polyline
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
            M.lookup hoveredMod (ttable._ttableBlockingUpstreamDependency)
          mkLine hoveredMod upMod
    linesToDownstream = maybe [] (fromMaybe [] . mkLinesToDownstream) mhoveredMod
      where
        mkLinesToDownstream :: ModuleName -> Maybe [Primitive TimingEvent]
        mkLinesToDownstream hoveredMod = do
          downMods <-
            M.lookup hoveredMod (ttable._ttableBlockedDownstreamDependency)
          pure $ mapMaybe (`mkLine` hoveredMod) downMods

buildMemChart ::
  forall m e.
  (MonadTextLayout m) =>
  -- | is it reverse-ordered w.r.t. memory allocation?
  Bool ->
  -- | offset for text
  Double ->
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  m (Scene (Primitive e))
buildMemChart isOrdered offsetForText drvModMap tui ttable = do
  renderedItems <- concat <$> traverse makeItem filteredItems
  pure
    Scene
      { sceneId = "mem-chart",
        sceneGlobalViewPort = ViewPort (0, 0) (120, timingHeight),
        sceneLocalViewPort = ViewPort (0, 0) (120, timingHeight),
        sceneElements = rules ++ renderedItems,
        sceneExtents = Nothing
      }
  where
    -- TODO: refactor these repeated part.
    timingInfos = ttable._ttableTimingInfos
    timingInfos' =
      fmap
        (\(drv_id, pinfo) -> (forwardLookup drv_id drvModMap, pinfo))
        timingInfos
    getMem (_, x) = maybe 0 (negate . memAllocCounter) (snd (x._plEnd))
    timingInfos''
      | not isOrdered = timingInfos'
      | otherwise =
          L.sortBy (flip compare `on` getMem) timingInfos'
    allItems = zip [0 ..] timingInfos''
    nMods = length allItems
    totalHeight = 5 * nMods
    rangeY =
      let vpi = tui._timingUIViewPort
          vp = fromMaybe (vpi._vpViewPort) (vpi._vpTempViewPort)
       in (snd vp.topLeft, snd vp.bottomRight)
    filteredItems = filter (`isInRange` rangeY) allItems
    alloc2X alloc =
      let -- ratio to 16 GiB
          allocRatio :: Double
          allocRatio = fromIntegral alloc / (16 * 1024 * 1024 * 1024)
       in allocRatio * span16G
    -- TODO: this 100 should be a customizable variable
    span16G :: Double
    span16G = 40
    rules =
      let xs = fmap (\x -> x / 16.0 * span16G) [0, 4 .. 32]
       in fmap (\x -> polyline (x, 0) [] (x, fromIntegral totalHeight) Gray 0.25) xs

    widthOfBox minfo = alloc2X (negate (memAllocCounter minfo))
    box color get_field (i, item) =
      case snd $ get_field $ snd item of
        Nothing -> []
        Just minfo ->
          [ rectangle
              (0, module2Y i)
              (widthOfBox minfo)
              3
              Nothing
              (Just color)
              Nothing
              Nothing
          ]
    moduleText (i, (mmodu, _)) = do
      let fontSize = 4
          moduTxt = fromMaybe "" mmodu
      drawText' (offsetForText, module2Y i + 3) LowerLeft Sans Black fontSize moduTxt
    makeItem x = do
      renderedText <- moduleText x
      if (tui._timingUIPartition)
        then
          pure $
            box LightSlateGray (._plEnd) x
              ++ box DeepSkyBlue (._plAs) x
              ++ box RoyalBlue (._plHscOut) x
              ++ [renderedText]
        else
          pure $
            box LightSlateGray (._plEnd) x
              ++ [renderedText]

buildTimingRange ::
  TimingUI ->
  TimingTable ->
  Scene (Primitive e)
buildTimingRange tui ttable =
  Scene
    { sceneId = "timing-range",
      sceneGlobalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight),
      sceneLocalViewPort = ViewPort (0, 0) (timingWidth, timingRangeHeight),
      sceneElements = [background, handle],
      sceneExtents = Nothing
    }
  where
    timingInfos = ttable._ttableTimingInfos
    nMods = length timingInfos
    allItems = zip [0 ..] timingInfos
    rangeY =
      let vpi = tui._timingUIViewPort
          vp = fromMaybe (vpi._vpViewPort) (vpi._vpTempViewPort)
       in (snd vp.topLeft, snd vp.bottomRight)
    filteredItems = filter (`isInRange` rangeY) allItems

    (minI, maxI) =
      let idxs = fmap fst filteredItems
       in (minimum idxs, maximum idxs)

    convert i = floor @Double (fromIntegral i / fromIntegral nMods * timingWidth)
    handleX :: Int
    handleX = if null filteredItems then 0 else convert minI
    handleWidth :: Int
    handleWidth = if null filteredItems then 0 else convert (maxI - minI + 1)

    background =
      rectangle
        (0, 0)
        timingWidth
        timingRangeHeight
        Nothing
        (Just LightGray)
        Nothing
        Nothing
    handle =
      rectangle
        (fromIntegral handleX, 0)
        (fromIntegral handleWidth)
        timingRangeHeight
        (Just Black)
        (Just White)
        (Just 1.0)
        Nothing

buildBlockers ::
  forall m e.
  (MonadTextLayout m) =>
  ModuleName ->
  TimingTable ->
  m (Scene (Primitive e))
buildBlockers hoveredMod ttable = do
  selected <- toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 hoveredMod
  let line = toSizedLine $ NE.singleton $ polyline (0, 0) [] (200, 0) Black 1
  blockedBy <- toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 "Blocked By"
  upstreams <- traverse (\t -> toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 t) upMods
  blocking <- toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 "Blocking"
  downstreams <- traverse (\t -> toSizedLine . NE.singleton <$> drawText' (0, 0) UpperLeft Sans Black 8 t) downMods
  let (vp, contentss) =
        flowLineByLine 0 $
          selected NE.:| ([line, blockedBy] ++ upstreams ++ [line, blocking] ++ downstreams)
      size = viewPortHeight vp
      box = rectangle (0, 0) 200 size (Just Black) (Just White) (Just 1.0) Nothing
  pure
    Scene
      { sceneId = "blockers",
        sceneGlobalViewPort = ViewPort (0, 0) (200, size),
        sceneLocalViewPort = ViewPort (0, 0) (200, size),
        sceneElements = box : F.toList (sconcat contentss),
        sceneExtents = Nothing
      }
  where
    upMods =
      maybeToList (M.lookup hoveredMod (ttable._ttableBlockingUpstreamDependency))
    downMods =
      fromMaybe [] (M.lookup hoveredMod (ttable._ttableBlockedDownstreamDependency))
