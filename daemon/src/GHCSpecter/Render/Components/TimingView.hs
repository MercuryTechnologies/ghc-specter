module GHCSpecter.Render.Components.TimingView
  ( -- * render
    render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    height,
    onMouseEnter,
    onMouseLeave,
    style,
    width,
  )
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.), (%~), _1, _2)
import Control.Monad (join)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
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
  ( HasPipelineInfo (..),
    HasTimingTable (..),
    TimingTable,
  )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Render.Util (divClass, xmlns)
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    hr,
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
  )
import GHCSpecter.UI.Types
  ( HasTimingUI (..),
    TimingUI,
  )
import GHCSpecter.UI.Types.Event
  ( ComponentTag (TimingBar, TimingView),
    Event (..),
    MouseEvent (..),
    TimingEvent (..),
  )
import Prelude hiding (div)

viewPortX :: TimingUI -> Int
viewPortX tui = tui ^. timingUIViewPortTopLeft . _1 . to floor

viewPortY :: TimingUI -> Int
viewPortY tui = tui ^. timingUIViewPortTopLeft . _2 . to floor

diffTime2X :: NominalDiffTime -> NominalDiffTime -> Double
diffTime2X totalTime time =
  realToFrac (time / totalTime) * timingMaxWidth

module2Y :: Double -> Double
module2Y i = 5.0 * i + 1.0

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

renderTimingChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderTimingChart drvModMap tui ttable =
  S.svg
    svgProps
    [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
    , S.g
        []
        ( renderRules (tui ^. timingUIHowParallel) ttable totalHeight totalTime
            ++ (fmap makeItem filteredItems)
            ++ lineToUpstream
            ++ linesToDownstream
        )
    ]
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
    makeItem x =
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

    timingInfos' = fmap (_1 %~ (`forwardLookup` drvModMap)) timingInfos
    allItems = zip [0 ..] timingInfos'
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

renderMemChart ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
renderMemChart drvModMap tui ttable =
  S.svg
    svgProps
    [ S.style [] [text ".small { font: 5px sans-serif; } text { user-select: none; }"]
    , S.g [] (fmap makeItem filteredItems)
    ]
  where
    timingInfos = ttable ^. ttableTimingInfos
    timingInfos' = fmap (_1 %~ (`forwardLookup` drvModMap)) timingInfos
    (i, _) `isInRange` (y0, y1) =
      let y = topOfBox i
       in y0 <= y && y <= y1

    allItems = zip [0 ..] timingInfos'
    filteredItems =
      filter (`isInRange` (viewPortY tui, viewPortY tui + timingHeight)) allItems

    nMods = length timingInfos
    totalHeight = 5 * nMods

    topOfBox :: Int -> Int
    topOfBox = floor . module2Y . fromIntegral

    widthOfBox minfo =
      let alloc = negate (memAllocCounter minfo)
          -- ratio to 4 GiB
          allocRatio :: Double
          allocRatio = fromIntegral alloc / fromIntegral (4 * 1024 * 1024 * 1024)
       in floor (allocRatio * 150) :: Int

    box (i, item) =
      case item ^. _2 . plEnd . _2 of
        Nothing -> []
        Just minfo ->
          [ S.rect
              ( [ SP.x "0"
                , SP.y (T.pack $ show (topOfBox i))
                , width (T.pack $ show (widthOfBox minfo))
                , height "3"
                , SP.fill "lightslategray"
                ]
              )
              []
          ]
    moduleText (i, item@(mmodu, _)) =
      let moduTxt = fromMaybe "" mmodu
       in S.text
            [ SP.x "150"
            , SP.y (T.pack $ show (topOfBox i + 3))
            , classList [("small", True)]
            ]
            [text moduTxt]
    makeItem x =
      S.g [] (box x ++ [moduleText x])


    viewboxProp =
      SP.viewBox . T.intercalate " " . fmap (T.pack . show) $
        [0, viewPortY tui, 300, timingHeight]
    svgProps =
      [ width "300"
      , height (T.pack (show timingHeight))
      , viewboxProp
      , SP.version "1.1"
      , xmlns
      ]


render ::
  BiKeyMap DriverId ModuleName ->
  TimingUI ->
  TimingTable ->
  Widget IHTML Event
render drvModMap tui ttable =
  divClass
    "box"
    []
    ( [ divClass
          "columns"
          []
          [ divClass
              "box column is-four-fifths"
              [style [("overflow", "hidden")]]
              [ renderTimingChart drvModMap tui ttable
              ]
          , divClass
              "box column is-one-fifth"
              [style [("overflow", "hidden")]]
              [renderMemChart drvModMap tui ttable]
          ]
      , renderTimingBar tui ttable
      ]
        ++ hoverInfo
    )
  where
      mhoveredMod = tui ^. timingUIHoveredModule
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
