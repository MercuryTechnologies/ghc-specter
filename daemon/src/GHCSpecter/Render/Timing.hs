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
    onMouseEnter,
    onMouseLeave,
    style,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), _1, _2)
import Control.Monad (join)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
  ( NominalDiffTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Data.Timing.Types
  ( HasTimingInfo (..),
    HasTimingTable (..),
    TimingTable,
  )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Render.Components.GraphView qualified as GraphView
import GHCSpecter.Render.Util (divClass, xmlns)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    HasTimingState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    hr,
    input,
    label,
    p,
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
renderTimingChart tui ttable =
  let timingInfos = ttable ^. ttableTimingInfos
      mhoveredMod = tui ^. timingUIHoveredModule
      nMods = length timingInfos
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

      box (i, item@(mmodu, _)) =
        let highlighter
              | mmodu == mhoveredMod = [SP.stroke "orange", SP.strokeWidth "0.5"]
              | otherwise = []
         in S.rect
              ( [ SP.x (T.pack $ show (leftOfBox item))
                , SP.y (T.pack $ show (topOfBox i))
                , width (T.pack $ show (widthOfBox item))
                , height "3"
                , SP.fill "lightslategray"
                ]
                  ++ highlighter
              )
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
        let moduTxt = fromMaybe "" mmodu
         in S.text
              [ SP.x (T.pack $ show (rightOfBox item))
              , SP.y (T.pack $ show (topOfBox i + 3))
              , classList [("small", True)]
              ]
              [text moduTxt]
      makeItems x =
        let props =
              case x of
                (_, (Nothing, _)) -> []
                (_, (Just modu, _)) ->
                  [ TimingEv (HoverOnModule modu) <$ onMouseEnter
                  , TimingEv (HoverOffModule modu) <$ onMouseLeave
                  ]
         in if (tui ^. timingUIPartition)
              then S.g props [box x, boxAs x, boxHscOut x, moduleText x]
              else S.g props [box x, moduleText x]
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
              | tui ^. timingUIHandleMouseMove =
                  [MouseEv TimingView . MouseMove <$> onMouseMove]
              | otherwise = []
         in mouseMove ++ prop1

      allItems = zip [0 ..] timingInfos
      filteredItems =
        filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

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

      svgElement =
        S.svg
          svgProps
          [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
          , S.g
              []
              ( renderRules (tui ^. timingUIHowParallel) ttable totalHeight totalTime
                  ++ (fmap makeItems filteredItems)
                  ++ lineToUpstream
                  ++ linesToDownstream
              )
          ]
   in divClass
        "box"
        [ style
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
    , buttonShowBlocker
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
    buttonShowBlocker = divClass "control" [] [button']
      where
        button'
          | tui ^. timingUIBlockerGraph =
              button [TimingEv CloseBlockerGraph <$ onClick] [text "Back to Timing Graph"]
          | otherwise =
              button [TimingEv ShowBlockerGraph <$ onClick] [text "Show Blocker Graph"]
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
renderTimingBar tui ttable =
  div [] [svgElement]
  where
    timingInfos = ttable ^. ttableTimingInfos
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

renderBlockerLine :: ModuleName -> TimingTable -> Widget IHTML Event
renderBlockerLine hoveredMod ttable =
  divClass "blocker" [] [selected, upstream, hr [], downstreams]
  where
    upMods =
      maybeToList (M.lookup hoveredMod (ttable ^. ttableBlockingUpstreamDependency))
    downMods =
      fromMaybe [] (M.lookup hoveredMod (ttable ^. ttableBlockedDownstreamDependency))

    selected =
      divClass "box" [] [p [] [text hoveredMod]]
    upstream =
      div
        []
        ( divClass "blocker title" [] [text "blocked by"] :
          fmap (\modu -> p [] [text modu]) upMods
        )
    downstreams =
      div
        []
        ( divClass "blocker title" [] [text "blocking"] :
          fmap (\modu -> p [] [text modu]) downMods
        )

-- | regular timing view mode
renderTimingMode :: UIModel -> ServerState -> Widget IHTML Event
renderTimingMode model ss =
  let ttable =
        fromMaybe
          (ss ^. serverTiming . tsTimingTable)
          (model ^. modelTiming . timingFrozenTable)
      mhoveredMod = model ^. modelTiming . timingUIHoveredModule
      hoverInfo =
        case mhoveredMod of
          Nothing -> []
          Just hoveredMod ->
            [ divClass
                "box"
                [ style
                    [ ("width", "150px")
                    , ("height", "120px")
                    , ("position", "absolute")
                    , ("bottom", "0")
                    , ("left", "0")
                    , ("background", "ivory")
                    , ("overflow", "hidden")
                    ]
                ]
                [renderBlockerLine hoveredMod ttable]
            ]
   in div
        [ style
            [ ("width", "100%")
            , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
            , ("position", "relative")
            ]
        ]
        ( [ renderTimingChart (model ^. modelTiming) ttable
          , div
              [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
              [renderCheckbox (model ^. modelTiming)]
          , renderTimingBar (model ^. modelTiming) ttable
          ]
            ++ hoverInfo
        )

renderBlockerGraph :: ServerState -> Widget IHTML Event
renderBlockerGraph ss =
  divClass
    "box"
    [ width (T.pack (show timingWidth))
    , height (T.pack (show timingHeight))
    , style [("overflow", "scroll")]
    ]
    contents
  where
    mblockerGraphViz = ss ^. serverTiming . tsBlockerGraphViz
    contents =
      case mblockerGraphViz of
        Nothing -> []
        Just blockerGraphViz ->
          [GraphView.renderGraph (const False) blockerGraphViz]

-- | blocker graph mode
renderBlockerGraphMode :: UIModel -> ServerState -> Widget IHTML Event
renderBlockerGraphMode model ss =
  div
    [ style
        [ ("width", "100%")
        , ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
        , ("position", "relative")
        , ("overflow", "auto")
        ]
    ]
    ( [ renderBlockerGraph ss
      , div
          [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
          [ renderCheckbox (model ^. modelTiming)
          ]
      ]
    )

render :: UIModel -> ServerState -> Widget IHTML Event
render model ss
  | model ^. modelTiming . timingUIBlockerGraph =
      renderBlockerGraphMode model ss
  | otherwise =
      renderTimingMode model ss
