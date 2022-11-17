{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCSpecter.Data.GHC.Orphans () where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Binary (Binary (..))
import GHC.RTS.Flags (
  CCFlags,
  ConcFlags,
  DebugFlags,
  DoCostCentres,
  DoHeapProfile,
  DoTrace,
  GCFlags,
  GiveGCStats,
  IoSubSystem (..),
  MiscFlags,
  ParFlags,
  ProfFlags,
  RTSFlags,
  TickyFlags,
  TraceFlags,
 )
import GHC.Stats (GCDetails, RTSStats)

-- orphan instances
instance Binary GiveGCStats

instance FromJSON GiveGCStats

instance ToJSON GiveGCStats

instance Binary GCFlags

instance FromJSON GCFlags

instance ToJSON GCFlags

instance Binary CCFlags

instance FromJSON CCFlags

instance ToJSON CCFlags

instance Binary ConcFlags

instance FromJSON ConcFlags

instance ToJSON ConcFlags

instance Binary DebugFlags

instance FromJSON DebugFlags

instance ToJSON DebugFlags

instance Binary DoCostCentres

instance FromJSON DoCostCentres

instance ToJSON DoCostCentres

instance Binary DoHeapProfile

instance FromJSON DoHeapProfile

instance ToJSON DoHeapProfile

instance Binary DoTrace

instance FromJSON DoTrace

instance ToJSON DoTrace

instance Binary IoSubSystem where
  put = put . fromEnum
  get = toEnum <$> get

instance FromJSON IoSubSystem where
  parseJSON =
    A.withText "IoSubSystem" $ \txt ->
      if
          | txt == "IoPOSIX" -> pure IoPOSIX
          | txt == "IoNative" -> pure IoNative
          | otherwise -> fail "cannot parse IoSubSystem"

instance ToJSON IoSubSystem where
  toJSON IoPOSIX = A.String "IoPOSIX"
  toJSON IoNative = A.String "IoNative"

instance Binary MiscFlags

instance FromJSON MiscFlags

instance ToJSON MiscFlags

instance Binary ParFlags

instance FromJSON ParFlags

instance ToJSON ParFlags

instance Binary ProfFlags

instance FromJSON ProfFlags

instance ToJSON ProfFlags

instance Binary TickyFlags

instance FromJSON TickyFlags

instance ToJSON TickyFlags

instance Binary TraceFlags

instance FromJSON TraceFlags

instance ToJSON TraceFlags

instance Binary RTSFlags

instance FromJSON RTSFlags

instance ToJSON RTSFlags

instance Binary GCDetails

instance FromJSON GCDetails

instance ToJSON GCDetails

instance Binary RTSStats

instance FromJSON RTSStats

instance ToJSON RTSStats
