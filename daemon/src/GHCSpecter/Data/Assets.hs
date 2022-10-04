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
import Data.Text.IO qualified as TIO
import Paths_ghc_specter_daemon (getDataFileName)

data Assets = Assets
  { _assetsGhcSpecterPng :: Text
  , _assetsGhcSpecterCss :: Text
  }

makeClassy ''Assets

loadAssets :: IO Assets
loadAssets = do
  -- load ghc-specter.png
  pngFile <- getDataFileName "assets/ghc-specter.png"
  bs <- B.readFile pngFile
  let b64 = B64.encode bs
      pngBase64 = "data:image/png;base64," <> TE.decodeUtf8 b64

  -- load ghc-specter.css
  cssFile <- getDataFileName "assets/ghc-specter.css"
  cssContent <- TIO.readFile cssFile

  let loadedAssets =
        Assets
          { _assetsGhcSpecterPng = pngBase64
          , _assetsGhcSpecterCss = cssContent
          }

  pure loadedAssets
