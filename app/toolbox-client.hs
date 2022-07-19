module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runClient "/tmp/ghc-build-analyzer.ipc" $ \s -> do
  sendAll s "Hello, world!"
  msg <- recv s 1024
  putStr "Received: "
  C.putStrLn msg

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient file client =
  withSocketsDo $
    E.bracket open close client
  where
    open = do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix file)
      pure sock
