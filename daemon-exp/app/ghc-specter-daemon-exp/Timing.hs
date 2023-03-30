{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Lens (to, (%~), (^.), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (
  NominalDiffTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
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
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..), TextPosition (..))
import GHCSpecter.Render.Components.TimingView (
  diffTime2X,
  module2Y,
  viewPortX,
  viewPortY,
 )
import GHCSpecter.UI.Constants (
  timingBarHeight,
  timingHeight,
  timingMaxWidth,
  timingWidth,
 )
import GHCSpecter.UI.Types (
  HasTimingUI (..),
  TimingUI (..),
 )
import GI.Cairo.Render qualified as R
import Types (ViewBackend)
import Util (renderPrimitive)

renderRules ::
  Bool ->
  TimingTable ->
  Int ->
  NominalDiffTime ->
  [Primitive]
renderRules _showParallel table totalHeight totalTime = fmap line ruleTimes
  where
    totalTimeInSec = nominalDiffTimeToSeconds totalTime
    ruleTimes = [0, 1 .. totalTimeInSec]
    _ranges = zip ruleTimes (tail ruleTimes)
    {- getParallelCompilation (sec1, sec2) =
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
      | otherwise = colorCodes !! 5 -}
    line sec =
      let x = fromIntegral (sec2X sec)
       in Polyline (x, 0) [] (x, fromIntegral totalHeight) Gray 0.25
    sec2X sec =
      floor (diffTime2X totalTime (secondsToNominalDiffTime sec)) :: Int

renderTimingChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  [Primitive]
renderTimingChart drvModMap tui ttable =
  renderRules (tui ^. timingUIHowParallel) ttable totalHeight totalTime
    ++ (concatMap makeItem filteredItems)
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
    topOfBox :: Int -> Int
    topOfBox = floor . module2Y . fromIntegral
    leftOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
       in floor (diffTime2X totalTime startTime) :: Int
    rightOfBox (_, tinfo) =
      let endTime = tinfo ^. plEnd . _1
       in floor (diffTime2X totalTime endTime) :: Int
    widthOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          endTime = tinfo ^. plEnd . _1
       in floor (diffTime2X totalTime (endTime - startTime)) :: Int
    widthHscOutOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          hscOutTime = tinfo ^. plHscOut . _1
       in floor (diffTime2X totalTime (hscOutTime - startTime)) :: Int
    widthAsOfBox (_, tinfo) =
      let startTime = tinfo ^. plStart . _1
          asTime = tinfo ^. plAs . _1
       in floor (diffTime2X totalTime (asTime - startTime)) :: Int
    (i, _) `isInRange` (y0, y1) =
      let y = topOfBox i
       in y0 <= y && y <= y1

    box (i, item@(mmodu, _)) =
      Rectangle (fromIntegral (leftOfBox item), fromIntegral (topOfBox i)) (fromIntegral (widthOfBox item)) 3 Nothing (Just Gray {- LightSlateGray -}) Nothing Nothing

    {-
     boxHscOut (i, item) =
       S.rect
         [ SP.x (T.pack $ show (leftOfBox item))
         , SP.y (T.pack $ show (topOfBox i))
         , width (T.pack $ show (widthHscOutOfBox item))
         , height "3"
         , SP.fill "royalblue"
         ]
         []
     boxAs (i, item) =
       S.rect
         [ SP.x (T.pack $ show (leftOfBox item))
         , SP.y (T.pack $ show (topOfBox i))
         , width (T.pack $ show (widthAsOfBox item))
         , height "3"
         , SP.fill "deepskyblue"
         ]
         [] -}
    moduleText (i, item@(mmodu, _)) =
      let fontSize = 4
          moduTxt = fromMaybe "" mmodu
       in DrawText (fromIntegral (rightOfBox item), fromIntegral (topOfBox i + 3)) LowerLeft Black fontSize moduTxt
    makeItem x = [box x, moduleText x]
    {-
          let mmodu = x ^. _2 . _1
              props =
                case mmodu of
                  Nothing -> []
                  Just modu ->
                    [ TimingEv (HoverOnModule modu) <$ onMouseEnter
                    , TimingEv (HoverOffModule modu) <$ onMouseLeave
                    ]
           in if (tui ^. timingUIPartition)
                then S.g props [box x, boxAs x, boxHscOut x, moduleText x]
                else S.g props [box x, moduleText x]
    -}
    timingInfos' = fmap (_1 %~ (`forwardLookup` drvModMap)) timingInfos
    allItems = zip [0 ..] timingInfos'
    filteredItems =
      filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

{-
    mkLine src tgt = do
      (srcIdx, srcItem) <-
        L.find (\(_, (mname, _)) -> mname == Just src) allItems
      (tgtIdx, tgtItem) <-
        L.find (\(_, (mname, _)) -> mname == Just tgt) allItems
      let line =
            S.line
              [ SP.x1 (T.pack $ show (leftOfBox srcItem))
              , SP.y1 (T.pack $ show (topOfBox srcIdx))
              , SP.x2 (T.pack $ show (rightOfBox tgtItem))
              , SP.y2 (T.pack $ show (topOfBox tgtIdx))
              , SP.stroke "red"
              , SP.strokeWidth "1"
              ]
              []
      pure line

    lineToUpstream = maybeToList $ join $ fmap mkLineToUpstream mhoveredMod
      where
        mkLineToUpstream hoveredMod = do
          upMod <-
            M.lookup hoveredMod (ttable ^. ttableBlockingUpstreamDependency)
          mkLine hoveredMod upMod

    linesToDownstream :: [Widget IHTML Event]
    linesToDownstream = maybe [] (fromMaybe [] . mkLinesToDownstream) mhoveredMod
      where
        mkLinesToDownstream :: ModuleName -> Maybe [Widget IHTML Event]
        mkLinesToDownstream hoveredMod = do
          downMods <-
            M.lookup hoveredMod (ttable ^. ttableBlockedDownstreamDependency)
          pure $ mapMaybe (`mkLine` hoveredMod) downMods
-}

renderTiming ::
  ViewBackend ->
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  R.Render ()
renderTiming vb drvModMap tui ttable = do
  let timingInfos = ttable ^. ttableTimingInfos
      nMods = length timingInfos
      modEndTimes = fmap (^. _2 . plEnd . _1) timingInfos
      totalHeight = 5 * nMods
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      rexp :: [Primitive]
      rexp = renderTimingChart drvModMap tui ttable
  traverse_ (renderPrimitive vb) rexp
