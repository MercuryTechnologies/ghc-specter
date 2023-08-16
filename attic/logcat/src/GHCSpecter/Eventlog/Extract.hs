{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHCSpecter.Eventlog.Extract
  ( InfoTableProvEntry (..),
    InfoTableMap,
    Chunk (..),
    getChunkedEvents,
    chunkStat,
    addStat,
    makeGraph,
    makeClosureInfoItem,
    extract,
  )
where

import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.List.Split (keepDelimsL, split, whenElt)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import GHC.Exts.Heap.ClosureTypes (ClosureType (..))
import GHC.RTS.Events
  ( Data (events),
    Event (evCap, evSpec, evTime),
    EventInfo (..),
    EventLog (..),
    Timestamp,
  )
import GHC.RTS.Events.Incremental (readEventLog)
import GHCSpecter.Eventlog.Types (ClosureInfoItem (..))
import Numeric (showHex)

data InfoTableProvEntry = IPE
  { ipeInfo :: Text,
    ipeTableName :: Text,
    ipeClosureDesc :: ClosureType,
    ipeTyDesc :: Text,
    ipeLabel :: Text,
    ipeModule :: Text,
    ipeSrcLoc :: Text
  }
  deriving (Show)

type InfoTableMap = HashMap Text InfoTableProvEntry

data SampleEntry = SampleEntry
  { seId :: Word8,
    seResidency :: Word64,
    seLabel :: Text
  }
  deriving (Show)

data Chunk = Chunk
  { chunkTimestamp :: Timestamp,
    chunkSamples :: [SampleEntry]
  }
  deriving (Show)

getChunkedEvents :: EventLog -> (InfoTableMap, [Chunk])
getChunkedEvents l =
  ( HM.fromList $ mapMaybe (toIPE . evSpec) infos,
    mapMaybe toChunk splitted
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
    (profs, others) = L.partition isHeapProf evs
    infos = filter isInfoTable others

    toIPE InfoTableProv {..} =
      let i = T.pack ("0x" ++ showHex itInfo "")
          ipe =
            IPE
              { ipeInfo = i,
                ipeTableName = itTableName,
                ipeClosureDesc = toEnum itClosureDesc,
                ipeTyDesc = itTyDesc,
                ipeLabel = itLabel,
                ipeModule = itModule,
                ipeSrcLoc = itSrcLoc
              }
       in Just (i, ipe)
    toIPE _ = Nothing

    toSE HeapProfSampleString {..} =
      Just $
        SampleEntry
          { seId = heapProfId,
            seResidency = heapProfResidency,
            seLabel = heapProfLabel
          }
    toSE _ = Nothing

    splitted = split (keepDelimsL $ whenElt isBegin) profs

    toChunk (start : samples) = Chunk (evTime start) <$> traverse (toSE . evSpec) samples
    toChunk _ = Nothing

addSample ::
  Timestamp ->
  HashMap Text (Timestamp, Word64) ->
  SampleEntry ->
  HashMap Text (Timestamp, Word64)
addSample t !acc SampleEntry {..} =
  let f Nothing = Just (t, seResidency)
      f (Just (_, v)) = Just (t, v + seResidency)
      !acc' = HM.alter f seLabel acc
   in acc'

chunkStat :: Chunk -> HashMap Text (Timestamp, Word64)
chunkStat c = L.foldl' (addSample (chunkTimestamp c)) HM.empty (chunkSamples c)

addStat :: HashMap Text [(Timestamp, Word64)] -> Chunk -> HashMap Text [(Timestamp, Word64)]
addStat !acc c =
  let stat1 = fmap L.singleton (chunkStat c)
      !acc' = HM.unionWith (++) acc stat1
   in acc'

makeGraph :: [Chunk] -> HashMap Text [(Timestamp, Word64)]
makeGraph cs = L.foldl' addStat HM.empty cs

makeClosureInfoItem :: InfoTableMap -> (Text, [(Timestamp, Word64)]) -> Maybe ClosureInfoItem
makeClosureInfoItem ipeMap (cid, graph) =
  Just $
    ClosureInfoItem
      { clsGraph = graph',
        -- TODO: find the way to get this info.
        clsN = 0,
        clsLabel = cid,
        clsDesc = maybe "" ipeTableName mipe,
        clsCTy = maybe "" (T.pack . show . ipeClosureDesc) mipe,
        clsType = maybe "" ipeTyDesc mipe,
        clsModule = maybe "" ipeModule mipe,
        clsLoc = maybe "" ipeSrcLoc mipe,
        clsSize = accSize
      }
  where
    toSec :: Timestamp -> Double
    toSec t = fromIntegral t / 1_000_000_000

    toMiB :: Word64 -> Double
    toMiB bytes = fromIntegral bytes / 1_048_576

    graph' = (0, 0) : fmap (\(t, bytes) -> (toSec t, toMiB bytes)) graph

    step !acc ((t0, b0), (t1, b1)) = acc + 0.5 * (t1 - t0) * (b0 + b1)

    -- trapezoidal sum
    accSize = L.foldl' step 0 (zip graph' (tail graph'))
    mipe = HM.lookup cid ipeMap

loadEventlog :: FilePath -> IO (Either String (EventLog, Maybe String))
loadEventlog fp = do
  lbs <- BL.readFile fp
  pure $ readEventLog lbs

extract :: FilePath -> IO [ClosureInfoItem]
extract fp = do
  eresult <- loadEventlog fp
  case eresult of
    Left err -> print err >> pure []
    Right (l, _) -> do
      let (infos, chunks) = getChunkedEvents l
          graphs = HM.toList $ makeGraph chunks
          clss = mapMaybe (makeClosureInfoItem infos) graphs
      pure clss
