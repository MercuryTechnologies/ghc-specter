module GHCSpecter.Channel.Inbound.Types
  ( Pause (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))

newtype Pause = Pause {unPause :: Bool}
  deriving (Eq, Ord, Show, Binary, FromJSON, ToJSON)
