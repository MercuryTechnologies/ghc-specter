module Toolbox.Render.Timing
  ( renderTiming,
    renderTimingChart,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    div,
    height,
    pre,
    text,
    textProp,
    width,
  )
import qualified Concur.Replica.SVG as S
import qualified Concur.Replica.SVG.Props as SP
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    secondsToNominalDiffTime,
  )
import Replica.VDOM.Types (HTML)
import Toolbox.Channel
  ( SessionInfo (..),
    Timer (..),
    type ModuleName,
  )
import Toolbox.Server.Types (ServerState (..))
import Prelude hiding (div)

xmlns :: Props a
xmlns = textProp "xmlns" "http://www.w3.org/2000/svg"

renderTimingChart :: [(ModuleName, (NominalDiffTime, NominalDiffTime))] -> Widget HTML a
renderTimingChart timingInfos =
  let nMods = length timingInfos
      modEndTimes = fmap (snd . snd) timingInfos
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      totalHeight = 5 * nMods
      topOfBox :: Int -> Int
      topOfBox i = 5 * i + 1
      leftOfBox (_, (startTime, _)) =
        floor (startTime / totalTime * 1024) :: Int
      widthOfBox (_, (startTime, endTime)) =
        floor ((endTime - startTime) / totalTime * 1024) :: Int
      box (i, item) =
        S.rect
          [ SP.x (T.pack $ show (leftOfBox item))
          , SP.y (T.pack $ show (topOfBox i))
          , width (T.pack $ show (widthOfBox item))
          , height "3"
          , SP.fill "red"
          ]
          []
      svgElement =
        S.svg
          [width "1024", height (T.pack $ show totalHeight), SP.version "1.1", xmlns]
          (fmap box $ zip [0 ..] timingInfos)
      infoElement =
        pre [] [text "text"]
   in div [] [svgElement, infoElement]

renderTiming :: ServerState -> Widget HTML a
renderTiming ss =
  case sessionStartTime (serverSessionInfo ss) of
    Nothing -> pre [] [text "GHC Session has not been started"]
    Just sessionStartTime ->
      let subtractTime (modName, Timer mstart mend) = do
            modStartTime <- mstart
            modEndTime <- mend
            let modStartTimeDiff = modStartTime `diffUTCTime` sessionStartTime
                modEndTimeDiff = modEndTime `diffUTCTime` sessionStartTime
            pure (modName, (modStartTimeDiff, modEndTimeDiff))
          timingInfos =
            L.sortOn (fst . snd) $ mapMaybe subtractTime $ M.toList $ serverTiming ss
       in renderTimingChart timingInfos
