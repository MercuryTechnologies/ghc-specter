{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Data.GHC.Hie
  ( -- * Row data for Hie
    RefRow' (..),
    DeclRow' (..),
    DefRow' (..),

    -- * Hie info per module
    ModuleHieInfo (..),
    emptyModuleHieInfo,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (type ModuleName)

-- | RefRow has OccName and Unit which are not JSON-serializable.
data RefRow' = RefRow'
  { _ref'Src :: FilePath,
    _ref'NameOcc :: Text,
    _ref'NameMod :: ModuleName,
    _ref'NameUnit :: Text,
    _ref'SLine :: Int,
    _ref'SCol :: Int,
    _ref'ELine :: Int,
    _ref'ECol :: Int
  }
  deriving (Show, Generic)

-- | DeclRow has OccName
data DeclRow' = DeclRow'
  { _decl'Src :: FilePath,
    _decl'NameOcc :: Text,
    _decl'SLine :: Int,
    _decl'SCol :: Int,
    _decl'ELine :: Int,
    _decl'ECol :: Int,
    _decl'Root :: Bool
  }
  deriving (Show, Generic)

-- | DefRow has OccName
data DefRow' = DefRow'
  { _def'Src :: FilePath,
    _def'NameOcc :: Text,
    _def'SLine :: Int,
    _def'SCol :: Int,
    _def'ELine :: Int,
    _def'ECol :: Int
  }
  deriving (Show, Generic)

data ModuleHieInfo = ModuleHieInfo
  { _modHieRefs :: [RefRow'],
    _modHieDecls :: [DeclRow'],
    _modHieDefs :: [DefRow'],
    _modHieSource :: Text
  }
  deriving (Show, Generic)

emptyModuleHieInfo :: ModuleHieInfo
emptyModuleHieInfo = ModuleHieInfo [] [] [] ""
