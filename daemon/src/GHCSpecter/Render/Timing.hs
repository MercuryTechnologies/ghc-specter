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
    height,
    onChange,
    onClick,
    style,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), _1, _2)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
  ( NominalDiffTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Data.Timing.Types
  ( HasTimingInfo (..),
    TimingTable,
  )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Render.Util (divClass, xmlns)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    input,
    label,
    text,
  )
import GHCSpecter.UI.ConcurReplica.DOM.Events
  ( onMouseDown,
    onMouseMove,
    onMouseUp,
  )
import GHCSpecter.UI.ConcurReplica.SVG qualified as S
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants
  ( timingBarHeight,
    timingHeight,
    timingMaxWidth,
    timingWidth,
    widgetHeight,
  )
import GHCSpecter.UI.Types
  ( HasTimingUI (..),
    HasUIModel (..),
    TimingUI,
    UIModel,
  )
import GHCSpecter.UI.Types.Event
  ( ComponentTag (TimingBar, TimingView),
    Event (..),
    MouseEvent (..),
    TimingEvent (..),
  )
import Prelude hiding (div)

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
  TimingTable ->
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
          filtered = filter (\x -> x ^. _2 . to (isTimeInTimerRange avg)) table
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
      floor (diffTime2X totalTime (secondsToNominalDiffTime sec)) :: Int
    box ((sec1, _), n) =
      S.rect
        [ SP.x (T.pack $ show $ sec2X sec1)
        , SP.y "0"
        , SP.width (T.pack $ show $ sec2X (1.01))
        , SP.height (T.pack $ show totalHeight)
        , SP.fill (nCPU2Color n)
        ]
        []

viewPortX :: TimingUI -> Int
viewPortX tui = tui ^. timingUIViewPortTopLeft . _1 . to floor

viewPortY :: TimingUI -> Int
viewPortY tui = tui ^. timingUIViewPortTopLeft . _2 . to floor

diffTime2X :: NominalDiffTime -> NominalDiffTime -> Double
diffTime2X totalTime time =
  realToFrac (time / totalTime) * timingMaxWidth

module2Y :: Double -> Double
module2Y i = 5.0 * i + 1.0

renderTimingChart ::
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingChart tui timingInfos =
  let nMods = length timingInfos
      modEndTimes = fmap (^. _2 . timingEnd) timingInfos
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      totalHeight = 5 * nMods
      topOfBox :: Int -> Int
      topOfBox = floor . module2Y . fromIntegral
      leftOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
         in floor (diffTime2X totalTime startTime) :: Int
      rightOfBox (_, tinfo) =
        let endTime = tinfo ^. timingEnd
         in floor (diffTime2X totalTime endTime) :: Int
      widthOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            endTime = tinfo ^. timingEnd
         in floor (diffTime2X totalTime (endTime - startTime)) :: Int
      widthHscOutOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            hscOutTime = tinfo ^. timingHscOut
         in floor (diffTime2X totalTime (hscOutTime - startTime)) :: Int
      widthAsOfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            asTime = tinfo ^. timingAs
         in floor (diffTime2X totalTime (asTime - startTime)) :: Int
      (i, _) `isInRange` (y0, y1) =
        let y = topOfBox i
         in y0 <= y && y <= y1
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
      moduleText (i, item@(mmodu, _)) =
        flip (maybe []) mmodu $ \modu ->
          [ S.text
              [ SP.x (T.pack $ show (rightOfBox item))
              , SP.y (T.pack $ show (topOfBox i + 3))
              , classList [("small", True)]
              ]
              [text modu]
          ]
      makeItems x
        | tui ^. timingUIPartition =
            [ box x
            , boxAs x
            , boxHscOut x
            ]
              ++ moduleText x
        | otherwise = [box x] ++ moduleText x
      svgProps =
        let viewboxProp =
              SP.viewBox . T.intercalate " " . fmap (T.pack . show) $
                [viewPortX tui, viewPortY tui, timingWidth, timingHeight]
            prop1 =
              [ MouseEv TimingView . MouseDown <$> onMouseDown
              , MouseEv TimingView . MouseUp <$> onMouseUp
              , width (T.pack (show timingWidth))
              , height (T.pack (show timingHeight))
              , viewboxProp
              , SP.version "1.1"
              , xmlns
              ]
            mouseMove
              | tui ^. timingUIHandleMouseMove = [MouseEv TimingView . MouseMove <$> onMouseMove]
              | otherwise = []
         in mouseMove ++ prop1

      allItems = zip [0 ..] timingInfos
      filteredItems =
        filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

      svgElement =
        S.svg
          svgProps
          [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
          , S.g
              []
              ( renderRules (tui ^. timingUIHowParallel) timingInfos totalHeight totalTime
                  ++ (concatMap makeItems filteredItems)
              )
          ]
   in div
        [ classList [("box", True)]
        , style
            [ ("width", T.pack (show timingWidth))
            , ("height", T.pack (show timingHeight))
            , ("overflow", "hidden")
            ]
        ]
        [svgElement]

renderCheckbox :: TimingUI -> Widget IHTML Event
renderCheckbox tui =
  div
    []
    [ buttonToCurrent
    , buttonFlow
    , checkPartition
    , checkHowParallel
    ]
  where
    isPartitioned = tui ^. timingUIPartition
    howParallel = tui ^. timingUIHowParallel
    mkEvent f b = TimingEv (f (not b)) <$ onChange
    buttonToCurrent =
      divClass
        "control"
        []
        [button [TimingEv ToCurrentTime <$ onClick] [text "To Current Time"]]
    buttonFlow = divClass "control" [] [button']
      where
        button'
          | isNothing (tui ^. timingFrozenTable) =
              button [TimingEv (TimingFlow False) <$ onClick] [text "Freeze"]
          | otherwise =
              button [TimingEv (TimingFlow True) <$ onClick] [text "Thaw"]
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
            , text "Parallelism"
            ]
        ]

renderTimingBar ::
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingBar tui timingInfos =
  div [] [svgElement]
  where
    nMods = length timingInfos

    topOfBox :: Int -> Int
    topOfBox = round . module2Y . fromIntegral

    (i, _) `isInRange` (y0, y1) =
      let y = topOfBox i
       in y0 <= y && y <= y1

    allItems = zip [0 ..] timingInfos
    filteredItems =
      filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

    (minI, maxI) =
      let idxs = fmap (^. _1) filteredItems
       in (minimum idxs, maximum idxs)

    convert i = floor @Double (fromIntegral i / fromIntegral nMods * fromIntegral timingWidth)
    handleX :: Int
    handleX = if null filteredItems then 0 else convert minI
    handleWidth :: Int
    handleWidth = if null filteredItems then 0 else convert (maxI - minI + 1)

    background =
      S.rect
        [ MouseEv TimingBar . MouseMove <$> onMouseMove
        , MouseEv TimingBar . MouseDown <$> onMouseDown
        , MouseEv TimingBar . MouseUp <$> onMouseUp
        , SP.x "0"
        , SP.y "0"
        , SP.width (T.pack (show timingWidth))
        , SP.height (T.pack (show timingBarHeight))
        , SP.fill "lightgray"
        ]
        []

    handle =
      S.rect
        [ SP.x (T.pack (show handleX))
        , SP.y "0"
        , SP.width (T.pack (show handleWidth))
        , SP.height (T.pack (show timingBarHeight))
        , SP.stroke "black"
        , SP.fill "white"
        ]
        []
    svgProps =
      [ width (T.pack (show timingWidth))
      , height (T.pack (show timingBarHeight))
      , SP.version "1.1"
      , xmlns
      ]

    svgElement =
      S.svg
        svgProps
        [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
        , background
        , handle
        ]

-- | Top-level render function for the Timing tab
render :: UIModel -> ServerState -> Widget IHTML Event
render model ss =
  let timingInfos =
        case model ^. modelTiming . timingFrozenTable of
          Nothing -> ss ^. serverTimingTable
          Just ttable -> ttable
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        [ renderTimingChart (model ^. modelTiming) timingInfos
        , div
            [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
            [renderCheckbox (model ^. modelTiming)]
        , renderTimingBar (model ^. modelTiming) timingInfos
        ]
