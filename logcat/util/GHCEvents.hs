module GHCEvents (
  -- *
  extract,

  -- *
  Chunk (..),
  getChunkedEvents,
) where

import Data.ByteString.Lazy qualified as BL
import Data.List.Split (keepDelimsL, split, whenElt)
import Data.Maybe (mapMaybe)
import GHC.RTS.Events (
  Data (events),
  Event (evCap, evSpec, evTime),
  EventLog (..),
  EventInfo (..),
  EventLog (header, dat),
  Timestamp,
 )
import GHC.RTS.Events.Incremental (readEventLog)

extract :: FilePath -> IO (Either String (EventLog, Maybe String))
extract fp = do
  lbs <- BL.readFile fp
  pure $ readEventLog lbs

data Chunk = Chunk
  { chunkTimestamp :: Timestamp
  , chunkEvents :: [(Timestamp, Event)]
  }
  deriving (Show)

getChunkedEvents :: EventLog -> [Chunk]
getChunkedEvents l =
  let evs = events (dat l)
      f e =
        let mx =
              case evSpec e of
                HeapProfSampleBegin {} -> Just e
                HeapProfSampleString {} -> Just e
                InfoTableProv {} -> Just e
                _ -> Nothing
         in (evTime e,) <$> mx
      getSec (t, _) = t `div` 1_000_000
      xs = mapMaybe f evs
      ys = fmap getSec xs

      isBegin (_, e) =
        case evSpec e of
          HeapProfSampleBegin {} -> True
          _ -> False
      splitted = split (keepDelimsL $ whenElt isBegin) xs

      toChunk ((t, _) : xs) = Chunk t xs
   in fmap toChunk splitted
