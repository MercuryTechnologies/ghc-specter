{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Timing.Types
  ( -- * extracted timing information
    TimingInfo (..),
    HasTimingInfo (..),
    type TimingTable,
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

type TimingTable = [(Maybe ModuleName, TimingInfo NominalDiffTime)]
