module GHCSpecter.Config
  ( Config (..),
    emptyConfig,
    defaultGhcSpecterConfigFile,
    loadConfig,
    withConfig,
  )
where

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Config = Config
  { configSocket :: FilePath,
    configSessionFile :: FilePath,
    configStartWithBreakpoint :: Bool,
    configModuleClusterSize :: Int
  }
  deriving (Show, Generic)

-- | default configuration
-- NOTE: non-trivial default value: cluster size = 150.
emptyConfig :: Config
emptyConfig =
  Config
    { configSocket = "/tmp/ghc-specter.ipc",
      configSessionFile = "",
      configStartWithBreakpoint = False {- True -},
      configModuleClusterSize = 65 -- 150
    }

defaultGhcSpecterConfigFile :: FilePath
defaultGhcSpecterConfigFile = "ghc-specter.yaml"

loadConfig :: FilePath -> IO (Either String Config)
loadConfig _fp = do
  -- TODO: for now
  pure (Right emptyConfig)

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg
