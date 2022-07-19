module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import Toolbox.Comm (runServer)

main :: IO ()
main = runServer "/tmp/ghc-build-analyzer.ipc" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (S.null msg) $ do
        sendAll s msg
        talk s
