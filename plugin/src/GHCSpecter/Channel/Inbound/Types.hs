module GHCSpecter.Channel.Inbound.Types
  ( InquireMessage (..),
    Request (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import GHC.Generics (Generic)

data InquireMessage = ListGlobalRdrElt
  deriving (Eq, Ord, Show, Generic)

instance Binary InquireMessage

instance FromJSON InquireMessage

instance ToJSON InquireMessage

data Request
  = Pause
  | Resume
  --   | Inquire InquireMessage
  deriving (Eq, Ord, Show, Generic)

instance Binary Request

instance FromJSON Request

instance ToJSON Request
