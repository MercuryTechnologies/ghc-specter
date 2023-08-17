{-# LANGUAGE BangPatterns #-}

module GHCSpecter.Comm
  ( Message (..),
    runServer,
    runClient,
    sendMessage,
    receiveMessage,
    sendObject,
    receiveObject,
  )
where

import Control.Concurrent (forkFinally)
import Control.Exception qualified as E
import Control.Monad (forever, replicateM, void)
import Data.Binary qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy.Char8 qualified as CL
import Data.Foldable (for_)
import Data.Word (Word32)
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
import Network.Socket.ByteString (recv, sendAll)

newtype Message = Message {unMessage :: C.ByteString}

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

chunksOf :: Int -> C.ByteString -> [C.ByteString]
chunksOf n bs = go [] bs
  where
    go !acc bs' =
      if C.length bs' <= n
        then acc ++ [bs']
        else
          let (bs1, bs2) = C.splitAt n bs'
           in go (acc ++ [bs1]) bs2

sendMessage :: Socket -> Message -> IO ()
sendMessage sock (Message !payload) = do
  let !sz :: Word32 = fromIntegral (C.length payload)
      !chunked = chunksOf 1024 payload
  -- NOTE: send and sendMany suffer from the vanishing resource problem.
  sendAll sock (CL.toStrict (B.encode sz))
  for_ chunked $ \chunk ->
    sendAll sock chunk

receiveMessage :: Socket -> IO Message
receiveMessage sock = do
  !sz :: Word32 <- B.decode . CL.fromStrict <$> recv sock 4
  let (n, m) = divMod sz 1024
  ps <- replicateM (fromIntegral n) (recv sock 1024)
  remainder <-
    if (m == 0)
      then pure []
      else do
        p <- recv sock (fromIntegral m)
        pure [p]
  let payload = C.concat (ps ++ remainder)
  pure (Message payload)

sendObject :: (B.Binary a) => Socket -> a -> IO ()
sendObject sock = sendMessage sock . Message . C.toStrict . B.encode

receiveObject :: (B.Binary a, Show a) => Socket -> IO a
receiveObject sock = do
  v <- (B.decode . CL.fromStrict . unMessage <$> receiveMessage sock)
  v `seq` pure v
