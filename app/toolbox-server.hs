module Main (main) where

import Concur.Core
import Concur.Replica
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
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Network.Socket.ByteString (recv)
import Toolbox.Comm (runServer)
import Prelude hiding (div)

main :: IO ()
main = do
  var <- atomically $ newTVar 0
  _ <- forkIO $ thread1 var
  _ <- forkIO $ webServer var
  -- _ <- forkIO webServer
  runServer "/tmp/ghc-build-analyzer.ipc" talk
  where
    talk s = do
      msgs' <- flip loopM [] $ \msgs -> do
        msg <- recv s 1024
        if C.null msg
          then pure $ Right msgs
          else pure $ Left (msgs ++ [msg])
      C.putStr (C.concat msgs')
      talk s

thread1 :: TVar Int -> IO ()
thread1 var = forever $ do
  val <- atomically $ readTVar var
  putStrLn $ "I am thread1: " ++ show val
  atomically $ modifyTVar' var (+ 1)
  getChar

webServer :: TVar Int -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" (\_ -> go initVal)
  where
    go val0 = do
      val <- liftIO $
        atomically $ do
          val' <- readTVar var
          if val0 == val'
            then retry
            else pure val'
      let txt = T.pack $ "I am thread2: " ++ show val
      div [] [text txt]
      go val

{-
webServer :: IO ()
webServer =
  runDefault 8080 "test" $ \_ ->
    div [] [ text "hello world" ]
-}
