module GHCSpecter.Render.Data.Assets
  ( ghcSpecterPng,
  )
where

import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Paths_ghc_specter_daemon (getDataFileName)
import System.IO ()

ghcSpecterPng :: IO Text
ghcSpecterPng = do
  pngFile <- getDataFileName "assets/ghc-specter.png"
  bs <- B.readFile pngFile
  let b64 = B64.encode bs
  pure $ "data:image/png;base64," <> TE.decodeUtf8 b64
