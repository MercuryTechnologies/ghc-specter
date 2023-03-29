{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import Control.Lens (to, (^.), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Time.Clock (
  NominalDiffTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import GHCSpecter.Data.Timing.Types (
  HasPipelineInfo (..),
  HasTimingTable (..),
  TimingTable,
 )
import GHCSpecter.Data.Timing.Util (isTimeInTimerRange)
import GHCSpecter.Graphics.DSL (Color (..), Primitive (..), TextPosition (..))
import GHCSpecter.Render.Components.TimingView (diffTime2X)
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

renderTiming ::
  ViewBackend ->
  TimingTable ->
  R.Render ()
renderTiming vb ttable = do
  let timingInfos = ttable ^. ttableTimingInfos
      nMods = length timingInfos
      modEndTimes = fmap (^. _2 . plEnd . _1) timingInfos
      totalHeight = 5 * nMods
      totalTime =
        case modEndTimes of
          [] -> secondsToNominalDiffTime 1 -- default time length = 1 sec
          _ -> maximum modEndTimes
      rexp :: [Primitive]
      rexp = renderRules False ttable totalHeight totalTime
  liftIO $ print ttable
  liftIO $ print totalTime
  liftIO $ print timingInfos
  liftIO $ print rexp
  traverse_ (renderPrimitive vb) rexp
