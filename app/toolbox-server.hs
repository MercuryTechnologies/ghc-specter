module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
  )
import Control.Monad (forever, unless)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv)
import Toolbox.Comm (runServer)

main :: IO ()
main = do
  var <- atomically $ newTVar 0
  _ <- forkIO $ thread1 var
  _ <- forkIO $ thread2 var
  runServer "/tmp/ghc-build-analyzer.ipc" talk
  where
    talk s = do
      msg <- recv s 1024
      unless (C.null msg) $ do
        C.putStr msg
        talk s

thread1 :: TVar Int -> IO ()
thread1 var = forever $ do
  putStrLn "I am thread1"
  atomically $ modifyTVar' var (+ 1)
  getChar

thread2 :: TVar Int -> IO ()
thread2 var = atomically (readTVar var) >>= go
  where
    go val0 = do
      val <- atomically $ do
        val' <- readTVar var
        if val0 == val'
          then retry
          else pure val'
      putStrLn $ "I am thread2: " ++ show val
      go val
