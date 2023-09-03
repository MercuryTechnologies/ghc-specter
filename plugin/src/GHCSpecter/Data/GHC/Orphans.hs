{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCSpecter.Data.GHC.Orphans () where

import Control.Monad (liftM2)
import Data.Binary (Binary (..))
import Data.Fixed (Pico)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock
  ( DiffTime,
    UTCTime (..),
    picosecondsToDiffTime,
  )
import GHC.Platform (PlatformMisc (..))
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
import GHC.Settings
  ( FileSettings (..),
    GhcNameVersion (..),
    Settings (..),
    ToolSettings (..),
  )
import GHC.Stats (GCDetails, RTSStats)
import GHC.Utils.CliOption (Option (..), showOpt)

-- orphan instances

-- UTCTime
instance Binary Day where
  get = fmap ModifiedJulianDay get
  put = put . toModifiedJulianDay

instance Binary DiffTime where
  get = fmap picosecondsToDiffTime get
  put = (put @Pico) . realToFrac

instance Binary UTCTime where
  get = liftM2 UTCTime get get
  put (UTCTime d dt) = put d >> put dt

-- Settings

instance Show Option where
  show = showOpt

deriving instance Show PlatformMisc

deriving instance Show ToolSettings

deriving instance Show FileSettings

deriving instance Show GhcNameVersion

deriving instance Show Settings

-- RTS
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
