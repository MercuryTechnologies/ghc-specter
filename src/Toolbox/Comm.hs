{-# OPTIONS_GHC -Werror #-}
module Toolbox.Comm
  ( Message (..),
    runServer,
    runClient,
    receiveMessage,
  )
where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.Extra (loopM)
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
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
import Network.Socket.ByteString (recv)

newtype Message = Message {unMessage :: Text}

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

-- using unix domain socket
runClient :: FilePath -> (Socket -> IO a) -> IO a
runClient file client =
  withSocketsDo $
    E.bracket open close client
  where
    open = do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix file)
      pure sock

receiveMessage :: Socket -> IO Message
receiveMessage sock = do
   msgs' <- flip loopM [] $ \msgs -> do
     msg <- recv sock 1024
     if C.null msg
       then pure $ Right msgs
       else pure $ Left (msgs ++ [msg])
   let txt = decodeUtf8 (C.concat msgs')
   pure (Message txt)
