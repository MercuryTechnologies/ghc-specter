module GHCSpecter.Config
  ( Config (..),
    emptyConfig,
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
where

import GHC.Generics (Generic)

data Config = Config
  { configSocket :: FilePath,
    configSessionFile :: FilePath,
    configWebPort :: Int,
    configStartWithBreakpoint :: Bool,
    configModuleClusterSize :: Int
  }
  deriving (Show, Generic)

-- | default configuration
-- NOTE: non-trivial default value: cluster size = 150.
emptyConfig :: Config
emptyConfig = Config "" "" 0 False 150

defaultGhcSpecterConfigFile :: FilePath
defaultGhcSpecterConfigFile = "ghc-specter.yaml"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig _fp = do
  -- TODO: for now
  pure (Right emptyConfig)
