module GHCSpecter.Channel.Inbound.Types (
  -- * subrequests
  SessionRequest (..),
  ConsoleRequest (..),

  -- * top-level request
  Request (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)

data SessionRequest
  = Pause
  | Resume
  | SetModuleBreakpoints [ModuleName]
  deriving (Eq, Ord, Show, Generic)

instance Binary SessionRequest

instance FromJSON SessionRequest

instance ToJSON SessionRequest

data ConsoleRequest
  = Ping Text
  | NextBreakpoint
  | ShowRenamed
  | ShowSplice
  | ShowExpr
  | ShowResult
  | ShowUnqualifiedImports
  | ListCore
  | PrintCore [Text]
  deriving (Eq, Ord, Show, Generic)

instance Binary ConsoleRequest

instance FromJSON ConsoleRequest

instance ToJSON ConsoleRequest

data Request
  = SessionReq SessionRequest
  | ConsoleReq DriverId ConsoleRequest
  deriving (Eq, Ord, Show, Generic)

instance Binary Request

instance FromJSON Request

instance ToJSON Request
