module GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    camelTo2,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToJSON,
  )
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Yaml qualified as Y
import GHC.Generics (Generic)

data Config = Config
  { configSocket :: FilePath
  , configSessionFile :: FilePath
  , configWebPort :: Int
  }
  deriving (Show, Generic)

modifier :: String -> String
modifier = camelTo2 '_' . drop 6

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = modifier}

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = modifier}

defaultGhcSpecterConfigFile :: FilePath
defaultGhcSpecterConfigFile = "ghc-specter.yaml"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig fp = do
  bs <- B.readFile fp
  pure $ first show $ Y.decodeEither' bs
