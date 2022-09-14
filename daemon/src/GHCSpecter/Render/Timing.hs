{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Render.Timing
  ( render,
    renderTimingChart,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    div,
    height,
    input,
    label,
    onChange,
    style,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG qualified as S
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), _2)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
  ( NominalDiffTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import GHCSpecter.Channel (type ModuleName)
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.ConcurReplica.DOM (text)
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasTimingUI (..),
    HasUIState (..),
    TimingUI,
    UIState,
  )
import GHCSpecter.UI.Types.Event
  ( Event (TimingEv),
    TimingEvent (..),
  )
import GHCSpecter.Util.Timing
  ( HasTimingInfo (..),
    TimingInfo,
    isInProgress,
    makeTimingTable,
  )
import Prelude hiding (div)

maxWidth :: (Num a) => a
maxWidth = 10240

colorCodes :: [Text]
colorCodes =
  [ "#EC7063"
  , "#F1948A"
  , "#F5B7B1"
  , "#FADBD8"
  , "#FDEDEC"
  , "#FFFFFF"
  ]

renderRules ::
  Bool ->
  [(ModuleName, TimingInfo NominalDiffTime)] ->
  Int ->
  NominalDiffTime ->
  [Widget IHTML a]
renderRules showParallel table totalHeight totalTime =
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
          filtered = filter (\x -> x ^. _2 . to (isInProgress avg)) table
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
      S.line
        [ SP.x1 (T.pack $ show $ sec2X sec)
        , SP.x2 (T.pack $ show $ sec2X sec)
        , SP.y1 "0"
        , SP.y2 (T.pack $ show totalHeight)
        , SP.stroke "gray"
        , SP.strokeWidth "0.25"
        ]
        []
    sec2X sec =
      floor (secondsToNominalDiffTime sec / totalTime * maxWidth) :: Int
    box ((sec1, _), n) =
      S.rect
        [ SP.x (T.pack $ show $ sec2X sec1)
        , SP.y "0"
        , SP.width (T.pack $ show $ sec2X (1.01))
        , SP.height (T.pack $ show totalHeight)
        , SP.fill (nCPU2Color n)
        ]
        []

renderTimingChart :: TimingUI -> [(ModuleName, TimingInfo NominalDiffTime)] -> Widget IHTML a
renderTimingChart tui timingInfos =
  let nMods = length timingInfos
      modEndTimes = fmap (^. _2 . timingEnd) timingInfos
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      totalHeight = 5 * nMods
      topOfBox :: Int -> Int
      topOfBox i = 5 * i + 1
      leftOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
         in floor (startTime / totalTime * maxWidth) :: Int
      rightOfBox (_, tinfo) =
        let endTime = tinfo ^. timingEnd
         in floor (endTime / totalTime * maxWidth) :: Int
      widthOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            endTime = tinfo ^. timingEnd
         in floor ((endTime - startTime) / totalTime * maxWidth) :: Int
      widthHscOutOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            hscOutTime = tinfo ^. timingHscOut
         in floor ((hscOutTime - startTime) / totalTime * maxWidth) :: Int
      widthAsOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            asTime = tinfo ^. timingAs
         in floor ((asTime - startTime) / totalTime * maxWidth) :: Int
      box (i, item) =
        S.rect
          [ SP.x (T.pack $ show (leftOfBox item))
          , SP.y (T.pack $ show (topOfBox i))
          , width (T.pack $ show (widthOfBox item))
          , height "3"
          , SP.fill "lightslategray"
          ]
          []
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
          []
      moduleText (i, item@(modu, _)) =
        S.text
          [ SP.x (T.pack $ show (rightOfBox item))
          , SP.y (T.pack $ show (topOfBox i + 3))
          , classList [("small", True)]
          ]
          [text modu]
      makeItems x
        | tui ^. timingUIPartition =
            [ box x
            , boxAs x
            , boxHscOut x
            , moduleText x
            ]
        | otherwise = [box x, moduleText x]
      svgElement =
        S.svg
          [width (T.pack $ show (maxWidth :: Int)), height (T.pack $ show totalHeight), SP.version "1.1", xmlns]
          ( S.style [] [text ".small { font: 5px sans-serif; }"] :
            ( renderRules (tui ^. timingUIHowParallel) timingInfos totalHeight totalTime
                ++ (concatMap makeItems $ zip [0 ..] timingInfos)
            )
          )
   in if tui ^. timingUISticky
        then
          div
            [style [("position", "absolute"), ("bottom", "0"), ("right", "0")]]
            [svgElement]
        else div [] [svgElement]

renderCheckbox :: TimingUI -> Widget IHTML Event
renderCheckbox tui = div [] [checkSticky, checkPartition, checkHowParallel]
  where
    isSticky = tui ^. timingUISticky
    isPartitioned = tui ^. timingUIPartition
    howParallel = tui ^. timingUIHowParallel
    mkEvent f b = TimingEv (f (not b)) <$ onChange
    checkSticky =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "sticky"
                , DP.checked isSticky
                , mkEvent UpdateSticky isSticky
                ]
            , text "Sticky"
            ]
        ]
    checkPartition =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "partition"
                , DP.checked isPartitioned
                , mkEvent UpdatePartition isPartitioned
                ]
            , text "Partition"
            ]
        ]
    checkHowParallel =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "howparallel"
                , DP.checked howParallel
                , mkEvent UpdateParallel howParallel
                ]
            , text "Parallel"
            ]
        ]

-- | Top-level render function for the Timing tab
render :: UIState -> ServerState -> Widget IHTML Event
render ui ss =
  let timingInfos = makeTimingTable ss
   in div
        [style [("width", "100%"), ("height", "100%"), ("position", "relative")]]
        [ renderTimingChart (ui ^. uiTiming) timingInfos
        , div
            [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
            [renderCheckbox (ui ^. uiTiming)]
        ]
