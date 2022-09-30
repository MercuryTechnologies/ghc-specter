module GHCSpecter.Channel.Inbound.Types
  ( Request (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import GHC.Generics (Generic)

data Request
  = Pause
  | Resume
  deriving (Eq, Ord, Show, Generic)

instance Binary Request

instance FromJSON Request

instance ToJSON Request
