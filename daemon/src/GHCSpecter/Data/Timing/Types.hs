{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Timing.Types
  ( -- * extracted timing information per module
    TimingInfo (..),
    HasTimingInfo (..),

    -- * collective timing information for the session
    TimingTable (..),
    HasTimingTable (..),
    emptyTimingTable,
  )
where

import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (ModuleName)

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingAs :: a
  , _timingEnd :: a
  }
  deriving (Show, Generic)

makeClassy ''TimingInfo

instance FromJSON a => FromJSON (TimingInfo a)

instance ToJSON a => ToJSON (TimingInfo a)

data TimingTable = TimingTable
  { _ttableTimingInfos :: [(Maybe ModuleName, TimingInfo NominalDiffTime)]
  }
  deriving (Show, Generic)

makeClassy ''TimingTable

emptyTimingTable :: TimingTable
emptyTimingTable = TimingTable []

instance FromJSON TimingTable

instance ToJSON TimingTable
