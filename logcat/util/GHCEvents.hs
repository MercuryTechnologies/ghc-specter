{-# LANGUAGE RecordWildCards #-}

module GHCEvents (
  -- *
  extract,

  -- *
  InfoTableProvEntry (..),
  InfoTableMap (..),
  Chunk (..),
  getChunkedEvents,
) where

import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (partition)
import Data.List.Split (keepDelimsL, split, whenElt)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.RTS.Events (
  Data (events),
  Event (evCap, evSpec, evTime),
  EventLog (..),
  EventInfo (..),
  EventLog (header, dat),
  Timestamp,
 )
import GHC.RTS.Events.Incremental (readEventLog)
import Numeric (showHex)

-- parse eventlog file
extract :: FilePath -> IO (Either String (EventLog, Maybe String))
extract fp = do
  lbs <- BL.readFile fp
  pure $ readEventLog lbs

data InfoTableProvEntry = IPE
  { ipeInfo :: Text
  , ipeTableName :: Text
  , ipeClosureDesc :: Int
  , ipeTyDesc :: Text
  , ipeLabel :: Text
  , ipeModule :: Text
  , ipeSrcLoc :: Text
  }
  deriving (Show)

type InfoTableMap = HashMap Text InfoTableProvEntry

data Chunk = Chunk
  { chunkTimestamp :: Timestamp
  , chunkSample :: [Event]
  }
  deriving (Show)

getChunkedEvents :: EventLog -> (InfoTableMap, [Chunk])
getChunkedEvents l =
  ( HM.fromList $ mapMaybe (toIPE . evSpec) infos
  , mapMaybe toChunk splitted
  )
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

    toIPE InfoTableProv {..} =
      let i = T.pack ("0x" ++ showHex itInfo "")
          ipe =
            IPE
              { ipeInfo = i
              , ipeTableName = itTableName
              , ipeClosureDesc = itClosureDesc
              , ipeTyDesc = itTyDesc
              , ipeLabel = itLabel
              , ipeModule = itModule
              , ipeSrcLoc = itSrcLoc
              }
       in Just (i, ipe)
    toIPE _ = Nothing
