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
    pre,
    text,
    width,
  )
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
    getEndTime,
    getHscOutTime,
    getStartTime,
    type ModuleName,
  )
import GHCSpecter.Render.Util (xmlns)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import Replica.VDOM.Types (HTML)
import Prelude hiding (div)

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingEnd :: a
  }

makeClassy ''TimingInfo

maxWidth :: (Num a) => a
maxWidth = 10240

renderTimingChart :: [(ModuleName, TimingInfo NominalDiffTime)] -> Widget HTML a
renderTimingChart timingInfos =
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
      width1OfBox (_, tinfo) =
        let startTime = tinfo ^. timingStart
            hscOutTime = tinfo ^. timingHscOut
         in floor ((hscOutTime - startTime) / totalTime * maxWidth) :: Int
      box (i, item) =
        S.rect
          [ SP.x (T.pack $ show (leftOfBox item))
          , SP.y (T.pack $ show (topOfBox i))
          , width (T.pack $ show (widthOfBox item))
          , height "3"
          , SP.fill "lightskyblue"
          ]
          []
      box1 (i, item) =
        S.rect
          [ SP.x (T.pack $ show (leftOfBox item))
          , SP.y (T.pack $ show (topOfBox i))
          , width (T.pack $ show (width1OfBox item))
          , height "3"
          , SP.fill "royalblue"
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
      svgElement =
        S.svg
          [width (T.pack $ show (maxWidth :: Int)), height (T.pack $ show totalHeight), SP.version "1.1", xmlns]
          ( S.style [] [text ".small { font: 5px sans-serif; }"] :
            ( fmap line [0, 1 .. totalTimeInSec]
                ++ (concatMap (\x -> [box x, box1 x, moduleText x]) $ zip [0 ..] timingInfos)
            )
          )
   in div [] [svgElement]

-- | Top-level render function for the Timing tab
render :: ServerState -> Widget HTML a
render ss =
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> pre [] [text "GHC Session has not been started"]
    Just sessionStartTime ->
      let subtractTime (modName, timer) = do
            modStartTime <- getStartTime timer
            modHscOutTime <- getHscOutTime timer
            modEndTime <- getEndTime timer
            let modStartTimeDiff = modStartTime `diffUTCTime` sessionStartTime
                modHscOutTimeDiff = modHscOutTime `diffUTCTime` sessionStartTime
                modEndTimeDiff = modEndTime `diffUTCTime` sessionStartTime
                tinfo = TimingInfo modStartTimeDiff modHscOutTimeDiff modEndTimeDiff
            pure (modName, tinfo)
          timingInfos =
            L.sortOn (^. _2 . timingStart) $ mapMaybe subtractTime $ M.toList $ ss ^. serverTiming
       in renderTimingChart timingInfos
