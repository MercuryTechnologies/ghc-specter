{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Data.Assets
  ( Assets (..),
    HasAssets (..),
    loadAssets,
  )
where

import Control.Lens (makeClassy)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Paths_ghc_specter_daemon (getDataFileName)
import System.IO ()

data Assets = Assets
  { _assetsGhcSpecterPng :: Text
  }

makeClassy ''Assets

loadAssets :: IO Assets
loadAssets = do
  pngFile <- getDataFileName "assets/ghc-specter.png"
  bs <- B.readFile pngFile
  let b64 = B64.encode bs
  pure
    Assets
      { _assetsGhcSpecterPng = "data:image/png;base64," <> TE.decodeUtf8 b64
      }
