module GHCEvents (
  -- *
  extract,

  -- *
  InfoTableMap (..),
  Chunk (..),
  getChunkedEvents,
) where

import Data.ByteString.Lazy qualified as BL
import Data.List (partition)
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

data InfoTableMap = InfoTableMap
  { infoTableMapContents :: [Event]
  }
  deriving (Show)

data Chunk = Chunk
  { chunkTimestamp :: Timestamp
  , chunkSample :: [Event]
  }
  deriving (Show)

getChunkedEvents :: EventLog -> (InfoTableMap, [Chunk])
getChunkedEvents l = (InfoTableMap infos, mapMaybeq toChunk splitted)
  where
    isHeapProf e =
      case evSpec e of
        HeapProfSampleBegin {} -> True
        HeapProfSampleString {} -> True
        _ -> False
    isBegin e =
      case evSpec e of
        HeapProfSampleBegin {} -> True
        _ -> False
    isInfoTable e =
      case evSpec e of
        InfoTableProv {} -> True
        _ -> False
    evs = events (dat l)
    (profs, others) = partition isHeapProf evs
    infos = filter isInfoTable others

    splitted = split (keepDelimsL $ whenElt isBegin) profs
    toChunk (start : samples) = Just (Chunk (evTime start) samples)
    toChunk _ = Nothing
