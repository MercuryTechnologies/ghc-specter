module GHCSpecter.Render.Timing
  ( render,
    renderTimingChart,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    div,
    height,
    pre,
    text,
    textProp,
    width,
  )
import Concur.Replica.SVG qualified as S
import Concur.Replica.SVG.Props qualified as SP
import Control.Lens (to, (^.))
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
    getStartTime,
    type ModuleName,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import Replica.VDOM.Types (HTML)
import Prelude hiding (div)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

maxWidth :: (Num a) => a
maxWidth = 10240

renderTimingChart :: [(ModuleName, (NominalDiffTime, NominalDiffTime))] -> Widget HTML a
renderTimingChart timingInfos =
  let nMods = length timingInfos
      modEndTimes = fmap (snd . snd) timingInfos
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      totalTimeInSec = nominalDiffTimeToSeconds totalTime
      totalHeight = 5 * nMods
      topOfBox :: Int -> Int
      topOfBox i = 5 * i + 1
      leftOfBox (_, (startTime, _)) =
        floor (startTime / totalTime * maxWidth) :: Int
      rightOfBox (_, (_, endTime)) =
        floor (endTime / totalTime * maxWidth) :: Int
      widthOfBox (_, (startTime, endTime)) =
        floor ((endTime - startTime) / totalTime * maxWidth) :: Int
      box (i, item) =
        S.rect
          [ SP.x (T.pack $ show (leftOfBox item))
          , SP.y (T.pack $ show (topOfBox i))
          , width (T.pack $ show (widthOfBox item))
          , height "3"
          , SP.fill "red"
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
                ++ (concatMap (\x -> [box x, moduleText x]) $ zip [0 ..] timingInfos)
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
            modEndTime <- getEndTime timer
            let modStartTimeDiff = modStartTime `diffUTCTime` sessionStartTime
                modEndTimeDiff = modEndTime `diffUTCTime` sessionStartTime
            pure (modName, (modStartTimeDiff, modEndTimeDiff))
          timingInfos =
            L.sortOn (fst . snd) $ mapMaybe subtractTime $ M.toList $ ss ^. serverTiming
       in renderTimingChart timingInfos
