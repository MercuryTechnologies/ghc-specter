{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Timing.Types
  ( -- * extracted information along the compilation pipeline of a given module
    PipelineInfo (..),
    HasPipelineInfo (..),

    -- * collective timing information for the session
    TimingTable (..),
    HasTimingTable (..),
    emptyTimingTable,
  )
where

import Control.Lens (makeClassy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (MemInfo)

-- | Information along the compilation pipeline for a single module
data PipelineInfo a = PipelineInfo
  { _plStart :: a,
    _plHscOut :: a,
    _plAs :: a,
    _plEnd :: a
  }
  deriving (Show, Generic, Functor)

makeClassy ''PipelineInfo

data TimingTable = TimingTable
  { -- | Start-time-ordered info table.
    _ttableTimingInfos :: [(DriverId, PipelineInfo (NominalDiffTime, Maybe MemInfo))],
    _ttableBlockingUpstreamDependency :: Map ModuleName ModuleName,
    _ttableBlockedDownstreamDependency :: Map ModuleName [ModuleName]
  }
  deriving (Show, Generic)

makeClassy ''TimingTable

emptyTimingTable :: TimingTable
emptyTimingTable = TimingTable [] M.empty M.empty
