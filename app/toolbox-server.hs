{-# OPTIONS_GHC -Werror #-}

module Main (main) where

import Concur.Core
  ( liftSTM,
  )
import Concur.Replica
  ( pre,
    runDefault,
    text,
  )
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVar,
    readTVar,
    retry,
    writeTVar,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Toolbox.Comm
  ( Message (..),
    receiveMessage,
    runServer,
  )
import Prelude hiding (div)

main :: IO ()
main = do
  var <- atomically $ newTVar Nothing
  _ <- forkIO $ listener var
  webServer var

listener :: TVar (Maybe (Int, Text)) -> IO ()
listener var =
  runServer
    "/tmp/ghc-build-analyzer.ipc"
    ( \sock -> do
        Message payload <- receiveMessage sock
        updateBuffer var (decodeUtf8 payload)
    )

updateBuffer :: TVar (Maybe (Int, Text)) -> Text -> IO ()
updateBuffer var txt = do
  val <- atomically $ readTVar var
  putStrLn $ "I am thread1: " ++ show val
  case val of
    Nothing -> atomically $ writeTVar var (Just (1, txt))
    Just (i, _) -> atomically $ writeTVar var (Just (i + 1, txt))

webServer :: TVar (Maybe (Int, Text)) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" (\_ -> go initVal)
  where
    go val0 = do
      let txt = maybe "Nothing" (\(i, t) -> T.pack (show i) <> ":\n" <> t) val0
      let action = do
            val <- liftSTM $ do
              val' <- readTVar var
              if fmap fst val0 == fmap fst val'
                then retry
                else pure val'
            go val
      (pre [] [text txt] <|> action)
