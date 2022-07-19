module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    gracefulClose,
    listen,
    maxListenQueue,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runServer "/tmp/ghc-build-analyzer.ipc" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (S.null msg) $ do
        sendAll s msg
        talk s

-- using unix domain socket
runServer :: FilePath -> (Socket -> IO a) -> IO a
runServer file server = withSocketsDo $ do
  -- addr <- resolve
  E.bracket open close loop
  where
    {- resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE]
              , addrFamily = AF_UNIX
              , addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) Nothing (Just port) -}
    open = do
      sock <- socket AF_UNIX Stream 0
      bind sock (SockAddrUnix file)
      listen sock maxListenQueue
      pure sock

    {- E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock -}
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
