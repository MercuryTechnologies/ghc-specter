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
import qualified Options.Applicative as OA
import Replica.VDOM.Types (HTML)
import Toolbox.Comm
  ( receiveObject,
    runServer,
  )
import Prelude hiding (div)

newtype Options = Options {optSocketFile :: FilePath}

optsParser :: OA.ParserInfo Options
optsParser =
  OA.info
    (Options <$> OA.strOption (OA.long "socket-file" <> OA.short 's' <> OA.help "socket file"))
    OA.fullDesc

main :: IO ()
main = do
  Options socketFile <- OA.execParser optsParser
  var <- atomically $ newTVar (0, mempty)
  _ <- forkIO $ listener socketFile var
  webServer var

type ModuleMessages = Map Text Text

listener :: FilePath -> TVar (Int, ModuleMessages) -> IO ()
listener socketFile var =
  runServer
    socketFile
    ( \sock -> do
        (modName, msg) <- receiveObject sock
        atomically $ updateModuleMessages var (modName, msg)
    )

updateModuleMessages :: TVar (Int, ModuleMessages) -> (Text, Text) -> STM ()
updateModuleMessages var (modName, msg) =
  modifyTVar' var $ \(i, m) -> (i + 1, M.insert modName msg m)

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

webServer :: TVar (Int, ModuleMessages) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" (\_ -> go initVal)
  where
    go (i, m) = do
      let widget
            | i == 0 = pre [] [text "No GHC process yet"]
            | otherwise = div [] [text (T.pack (show i)), renderModuleMessages m]
      let await = liftSTM $ do
            (i', m') <- readTVar var
            if i == i'
              then retry
              else pure (i', m')
      val <- (widget <|> await)
      go val
