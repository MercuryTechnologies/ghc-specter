{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCSpecter.Data.GHC.Orphans () where

import Data.Binary (Binary (..))
import GHC.RTS.Flags
  ( CCFlags,
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

instance Binary GCFlags

instance Binary CCFlags

instance Binary ConcFlags

instance Binary DebugFlags

instance Binary DoCostCentres

instance Binary DoHeapProfile

instance Binary DoTrace

instance Binary IoSubSystem where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary MiscFlags

instance Binary ParFlags

instance Binary ProfFlags

instance Binary TickyFlags

instance Binary TraceFlags

instance Binary RTSFlags

instance Binary GCDetails

instance Binary RTSStats
