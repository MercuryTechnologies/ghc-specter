{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Hie
  ( -- * Row data for Hie
    RefRow' (..),
    HasRefRow' (..),
    DeclRow' (..),
    HasDeclRow' (..),
    DefRow' (..),
    HasDefRow' (..),

    -- * Hie info per module
    ModuleHieInfo (..),
    HasModuleHieInfo (..),
    emptyModuleHieInfo,
  )
where

import Control.Lens (makeClassy)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (type ModuleName)

-- | RefRow has OccName and Unit which are not JSON-serializable.
data RefRow' = RefRow'
  { _ref'Src :: FilePath
  , _ref'NameOcc :: Text
  , _ref'NameMod :: ModuleName
  , _ref'NameUnit :: Text
  , _ref'SLine :: Int
  , _ref'SCol :: Int
  , _ref'ELine :: Int
  , _ref'ECol :: Int
  }
  deriving (Show, Generic)

makeClassy ''RefRow'

instance FromJSON RefRow'

instance ToJSON RefRow'

-- | DeclRow has OccName
data DeclRow' = DeclRow'
  { _decl'Src :: FilePath
  , _decl'NameOcc :: Text
  , _decl'SLine :: Int
  , _decl'SCol :: Int
  , _decl'ELine :: Int
  , _decl'ECol :: Int
  , _decl'Root :: Bool
  }
  deriving (Show, Generic)

makeClassy ''DeclRow'

instance FromJSON DeclRow'

instance ToJSON DeclRow'

-- | DefRow has OccName
data DefRow' = DefRow'
  { _def'Src :: FilePath
  , _def'NameOcc :: Text
  , _def'SLine :: Int
  , _def'SCol :: Int
  , _def'ELine :: Int
  , _def'ECol :: Int
  }
  deriving (Show, Generic)

makeClassy ''DefRow'

instance FromJSON DefRow'

instance ToJSON DefRow'

data ModuleHieInfo = ModuleHieInfo
  { _modHieRefs :: [RefRow']
  , _modHieDecls :: [DeclRow']
  , _modHieDefs :: [DefRow']
  , _modHieSource :: Text
  }
  deriving (Show, Generic)

makeClassy ''ModuleHieInfo

instance FromJSON ModuleHieInfo

instance ToJSON ModuleHieInfo

emptyModuleHieInfo :: ModuleHieInfo
emptyModuleHieInfo = ModuleHieInfo [] [] [] ""
