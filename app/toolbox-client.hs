module Main (main) where

import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Toolbox.Comm (runClient)

main :: IO ()
main = runClient "/tmp/ghc-build-analyzer.ipc" $ \s -> do
  sendAll s "Hello, world!"
  msg <- recv s 1024
  putStr "Received: "
  C.putStrLn msg
