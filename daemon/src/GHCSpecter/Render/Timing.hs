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
    onInput,
    pre,
    style,
    text,
    width,
  )
import Concur.Replica.DOM.Props qualified as DP (checked, name, type_)
import Concur.Replica.SVG qualified as S
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (makeClassy, to, (^.), _2)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import GHCSpecter.Channel
  ( SessionInfo (..),
    getAsTime,
    getEndTime,
    getHscOutTime,
    getStartTime,
    type ModuleName,
  )
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.Server.Types
  ( Event (TimingEv),
    HasServerState (..),
    HasTimingUI (..),
    HasUIState (..),
    ServerState (..),
    TimingEvent (..),
    TimingUI,
    UIState,
  )
import Replica.VDOM.Types (HTML)
import Prelude hiding (div)

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingAs :: a
  , _timingEnd :: a
  }
  deriving (Show)

makeClassy ''TimingInfo

maxWidth :: (Num a) => a
maxWidth = 10240

renderTimingChart :: TimingUI -> [(ModuleName, TimingInfo NominalDiffTime)] -> Widget HTML a
renderTimingChart tui timingInfos =
  let nMods = length timingInfos
      modEndTimes = fmap (^. _2 . timingEnd) timingInfos
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      totalTimeInSec = nominalDiffTimeToSeconds totalTime
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
      sec2X sec =
        floor (secondsToNominalDiffTime sec / totalTime * maxWidth) :: Int
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
            ( fmap line [0, 1 .. totalTimeInSec]
                ++ (concatMap makeItems $ zip [0 ..] timingInfos)
            )
          )
   in if tui ^. timingUISticky
        then
          div
            [style [("position", "absolute"), ("bottom", "0"), ("right", "0")]]
            [svgElement]
        else div [] [svgElement]

renderCheckbox :: TimingUI -> Widget HTML Event
renderCheckbox tui = div [] [checkSticky, checkPartition]
  where
    isSticky = tui ^. timingUISticky
    isPartitioned = tui ^. timingUIPartition
    checkSticky =
      div
        [classList [("control", True)]]
        [ label
            [classList [("checkbox", True)]]
            [ input
                [ DP.type_ "checkbox"
                , DP.name "sticky"
                , DP.checked isSticky
                , TimingEv (UpdateSticky (not isSticky)) <$ onInput
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
                , TimingEv (UpdatePartition (not isPartitioned)) <$ onInput
                ]
            , text "Partition"
            ]
        ]

-- | Top-level render function for the Timing tab
render :: UIState -> ServerState -> Widget HTML Event
render ui ss =
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> pre [] [text "GHC Session has not been started"]
    Just sessionStartTime ->
      let subtractTime (modName, timer) = do
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
          timingInfos =
            L.sortOn (^. _2 . timingStart) $ mapMaybe subtractTime $ M.toList $ ss ^. serverTiming
       in div
            [style [("width", "100%"), ("height", "100%"), ("position", "relative")]]
            [ renderTimingChart (ui ^. uiTiming) timingInfos
            , div
                [style [("position", "absolute"), ("top", "0"), ("right", "0")]]
                [renderCheckbox (ui ^. uiTiming)]
            ]
