module Toolbox.Comm where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    connect,
    gracefulClose,
    listen,
    maxListenQueue,
    socket,
    withSocketsDo,
  )

-- using unix domain socket
runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer file server = withSocketsDo $ E.bracket open close loop
  where
    open = do
      sock <- socket AF_UNIX Stream 0
      bind sock (SockAddrUnix file)
      listen sock maxListenQueue
      pure sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)

runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient file client =
  withSocketsDo $
    E.bracket open close client
  where
    open = do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix file)
      pure sock
