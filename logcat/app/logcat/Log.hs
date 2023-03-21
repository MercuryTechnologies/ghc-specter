{-# LANGUAGE OverloadedStrings #-}

module Log (
  recordEvent,
  dumpLog,
  flushEventQueue,
) where

import Control.Lens ((%~), (&), (.~), (^.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (toList)
import Data.List qualified as L (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq (empty)
import GHC.RTS.Events (Event (..), Header)
import GHC.RTS.Events.Incremental (
  Decoder (..),
  decodeEvents,
  readHeader,
 )
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Render.Timeline qualified as Timeline
import Render.Util (
  canvasWidth,
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
  | ltimePos > canvasWidth - Timeline.timelineMargin =
      let currCenterTime = Timeline.pixelToSec origin (canvasWidth * 0.5)
          deltaTime = ltime - currCenterTime
       in (logcatViewState . viewTimelineOrigin %~ (\x -> x + deltaTime)) s
  | otherwise = s
  where
    origin = s ^. logcatViewState . viewTimelineOrigin
    ltime = s ^. logcatLastEventTime
    ltimePos = Timeline.secToPixel origin ltime

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

dumpLog :: (Event -> IO ()) -> (Int -> IO ()) -> Socket -> IO ()
dumpLog postEventAction postReceiveAction sock = goHeader ""
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
              bs' = BL.toStrict lbs'
              nBytes0 = BS.length bs'
          goEvents hdr dec0 nBytes0 (BL.toStrict lbs')

    go :: Decoder Event -> BS.ByteString -> IO (Maybe (Decoder Event), BS.ByteString)
    go dec !bytes = do
      case dec of
        Produce ev dec' -> do
          postEventAction ev
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

    goEvents :: Header -> Decoder Event -> Int -> BS.ByteString -> IO ()
    goEvents hdr dec !nBytes !bytes = do
      (mdec', bytes') <- go dec bytes
      let dec' = fromMaybe (decodeEvents hdr) mdec'
      bytes'' <- recv sock 1024
      let nBytes' = nBytes + BS.length bytes''
      postReceiveAction nBytes'
      goEvents hdr dec' nBytes' (bytes' <> bytes'')
