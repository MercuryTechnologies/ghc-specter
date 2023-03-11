{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Log (
  recordEvent,
  dumpLog,
  flushEventQueue,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Control.Lens ((%~), (&), (.~), (^.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (toList)
import Data.List qualified as L (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq (empty)
import GHC.RTS.Events (Event (..))
import GHC.RTS.Events.Incremental (
  Decoder (..),
  decodeEvents,
  readHeader,
 )
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Render (
  canvasWidth,
  pixelToSec,
  secToPixel,
  timelineMargin,
 )
import System.IO (hFlush, stdout)
import Text.Pretty.Simple (pPrint)
import Types (
  HasLogcatState (..),
  HasViewState (..),
  LogcatState,
 )
import Util.Event (eventInfoToString)
import Util.Histo (aggregateCount, histoAdd)

-- | Adjust timeline viewport to ensure the last event is out of the right margin
-- of the timeline. This checks if the last event falls under the margin, and if so,
-- move the plot origin to make the last event at the center of the timeline.
adjustTimelineOrigin :: LogcatState -> LogcatState
adjustTimelineOrigin s
  | ltimePos > canvasWidth - timelineMargin =
      let currCenterTime = pixelToSec origin (canvasWidth * 0.5)
          deltaTime = ltime - currCenterTime
       in (logcatViewState . viewTimeOrigin %~ (\x -> x + deltaTime)) s
  | otherwise = s
  where
    origin = s ^. logcatViewState . viewTimeOrigin
    ltime = s ^. logcatLastEventTime
    ltimePos = secToPixel origin ltime

recordEvent :: Event -> LogcatState -> LogcatState
recordEvent ev s =
  let ltime = s ^. logcatLastEventTime
      sec = MkFixed (fromIntegral (evTime ev))
      updateLastEventTime =
        if sec > ltime
          then logcatLastEventTime .~ sec
          else id
      s' = s & ((logcatEventQueue %~ (|> ev)) . updateLastEventTime)
   in s'

flushEventQueue :: LogcatState -> (Bool, LogcatState)
flushEventQueue s =
  let queue = s ^. logcatEventQueue
      hist = s ^. logcatEventHisto
      diff = aggregateCount $ fmap (eventInfoToString . evSpec) $ toList queue
      hist' = L.foldl' histoAdd hist diff
      s' =
        s
          & (logcatEventStore %~ (<> queue))
            . (logcatEventQueue .~ Seq.empty)
            . (logcatEventHisto .~ hist')
            . adjustTimelineOrigin
   in (True, s')

dumpLog :: (Event -> IO ()) -> Socket -> IO ()
dumpLog action sock = goHeader ""
  where
    goHeader bs0 = do
      bs1 <- recv sock 1024
      let bs = bs0 <> bs1
      let lbs = BL.fromStrict bs
      let e = readHeader lbs
      case e of
        Left err -> print err >> goHeader bs
        Right (hdr, lbs') -> do
          pPrint hdr
          let dec0 = decodeEvents hdr
          goEvents hdr dec0 (BL.toStrict lbs')

    go :: Decoder Event -> BS.ByteString -> IO (Maybe (Decoder Event), BS.ByteString)
    go dec !bytes = do
      case dec of
        Produce ev dec' -> do
          action ev
          hFlush stdout
          go dec' bytes
        Consume k ->
          if BS.null bytes
            then pure (Just dec, "")
            else go (k bytes) ""
        Done bytes' ->
          pure (Nothing, bytes')
        Error _bytes' e -> do
          pPrint e
          hFlush stdout
          -- reset if error happens.
          pure (Nothing, "")

    goEvents hdr dec !bytes = do
      (mdec', bytes') <- go dec bytes
      let dec' = fromMaybe (decodeEvents hdr) mdec'
      bytes'' <- recv sock 1024
      goEvents hdr dec' (bytes' <> bytes'')
