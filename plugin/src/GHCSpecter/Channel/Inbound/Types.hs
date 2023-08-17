module GHCSpecter.Channel.Inbound.Types
  ( -- * subrequests
    SessionRequest (..),
    ConsoleRequest (..),

    -- * top-level request
    Request (..),
  )
where

import Data.Binary (Binary (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)

data SessionRequest
  = Pause
  | Resume
  | SetModuleBreakpoints [ModuleName]
  | ExitGhcDebug
  deriving (Eq, Ord, Show, Generic)

instance Binary SessionRequest

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
  | DumpHeap
  deriving (Eq, Ord, Show, Generic)

instance Binary ConsoleRequest

data Request
  = SessionReq SessionRequest
  | ConsoleReq DriverId ConsoleRequest
  deriving (Eq, Ord, Show, Generic)

instance Binary Request
