module Main (main) where

import Concur.Core
  ( Widget,
    liftSTM,
  )
import Concur.Replica
  ( div,
    pre,
    runDefault,
    text,
  )
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
  )
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Comm
  ( receiveObject,
    runServer,
  )
import Prelude hiding (div)

main :: IO ()
main = do
  var <- atomically $ newTVar Nothing
  _ <- forkIO $ listener var
  webServer var

type ModuleMessages = Map Text Text

listener :: TVar (Maybe (Int, ModuleMessages)) -> IO ()
listener var =
  runServer
    "/tmp/ghc-build-analyzer.ipc"
    ( \sock -> do
        (modName, msg) <- receiveObject sock
        atomically $ updateModuleMessages var (modName, msg)
    )

updateModuleMessages :: TVar (Maybe (Int, ModuleMessages)) -> (Text, Text) -> STM ()
updateModuleMessages var (modName, msg) = modifyTVar' var $ \mmap ->
  case mmap of
    Nothing -> Just (1, M.singleton modName msg)
    Just (i, m) -> Just (i + 1, M.insert modName msg m)

renderModuleMessages :: ModuleMessages -> Widget HTML a
renderModuleMessages m =
  div [] $ map eachRender $ M.toList m
  where
    eachRender :: (Text, Text) -> Widget HTML a
    eachRender (k, v) =
      div
        []
        [ text ("module: " <> k)
        , pre [] [text v]
        , pre [] [text "-----------"]
        ]

webServer :: TVar (Maybe (Int, ModuleMessages)) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" (\_ -> go initVal)
  where
    go val0 = do
      let widget =
            maybe
              (pre [] [text "No GHC process yet"])
              (\(i, m) -> div [] [text (T.pack (show i)), renderModuleMessages m])
              val0
      let await = liftSTM $ do
            val' <- readTVar var
            if fmap fst val0 == fmap fst val'
              then retry
              else pure val'
      val <- (widget <|> await)
      go val
