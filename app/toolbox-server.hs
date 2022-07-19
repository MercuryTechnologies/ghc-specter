module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv)
import Toolbox.Comm (runServer)

main :: IO ()
main = runServer "/tmp/ghc-build-analyzer.ipc" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (C.null msg) $ do
        C.putStr msg
        talk s
