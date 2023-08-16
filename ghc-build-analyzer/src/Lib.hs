module Lib where

import Data.Aeson as A
import Data.Aeson.Types as A

-- | A JSON message produced by GHC.
--
-- NB. I've never seen the `msgSpan` or `reason` fields be anything but @null@.
data GhcMessage = GhcMessage
  { msgSpan :: Maybe String,
    doc :: String,
    severity :: Severity,
    reason :: Maybe String
  }
  deriving (Show)

instance FromJSON GhcMessage where
  parseJSON (Object v) = do
    msgSpan <- v .: "span"
    doc <- v .: "doc"
    severity <- v .: "severity"
    reason <- v .: "reason"
    return GhcMessage {msgSpan, doc, severity, reason}
  parseJSON invalid =
    prependFailure
      "Parsing GhcMessage failed, "
      (typeMismatch "Object" invalid)

data Severity
  = SevInfo
  | SevInteractive
  | SevOutput
  deriving (Show)

instance FromJSON Severity where
  parseJSON (A.String "SevInfo") = return SevInfo
  parseJSON (A.String "SevInteractive") = return SevInteractive
  parseJSON (A.String "SevOutput") = return SevOutput
  parseJSON invalid =
    prependFailure
      "Parsing severity failed, "
      (typeMismatch "String" invalid)
