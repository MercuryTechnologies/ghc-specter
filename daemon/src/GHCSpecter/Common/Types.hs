{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Common.Types
  ( -- * extracted timing information
    TimingInfo (..),
    HasTimingInfo (..),
    type TimingTable,
  )
where

import Control.Lens (makeClassy)
import Data.Time.Clock (NominalDiffTime)
import GHCSpecter.Channel.Common.Types (ModuleName)

data TimingInfo a = TimingInfo
  { _timingStart :: a
  , _timingHscOut :: a
  , _timingAs :: a
  , _timingEnd :: a
  }
  deriving (Show)

makeClassy ''TimingInfo

type TimingTable = [(Maybe ModuleName, TimingInfo NominalDiffTime)]
