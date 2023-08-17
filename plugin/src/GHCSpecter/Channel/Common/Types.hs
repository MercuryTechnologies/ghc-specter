{-# LANGUAGE ExplicitNamespaces #-}

module GHCSpecter.Channel.Common.Types
  ( type ModuleName,
    DriverId (..),
  )
where

import Data.Binary (Binary)
import Data.Text (Text)

type ModuleName = Text

newtype DriverId = DriverId {unDriverId :: Int}
  deriving (Show, Eq, Ord, Num, Binary)
