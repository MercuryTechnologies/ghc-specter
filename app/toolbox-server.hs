{-# LANGUAGE GADTs #-}

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Options.Applicative as OA
import Replica.VDOM.Types (HTML)
import Toolbox.Channel
  ( ChanMessage (CMCheckImports, CMTrivial),
    ChanMessageBox (..),
  )
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

type ChanModule = (Text, Text)

type Inbox = Map ChanModule Text

listener :: FilePath -> TVar (Int, Inbox) -> IO ()
listener socketFile var =
  runServer
    socketFile
    (\sock -> receiveObject sock >>= atomically . updateInbox var)

updateInbox :: TVar (Int, Inbox) -> ChanMessageBox -> STM ()
updateInbox var chanMsg =
  modifyTVar' var $ \(i, m) ->
    let (chan, modu, msg) =
          case chanMsg of
            CMBox (CMCheckImports m' t') -> ("check-imports", m', t')
            CMBox (CMTrivial m') -> ("trivial", m', "no-message")
     in (i + 1, M.insert (chan, modu) msg m)

renderInbox :: Inbox -> Widget HTML a
renderInbox m =
  div [] $ map eachRender $ M.toList m
  where
    eachRender :: (ChanModule, Text) -> Widget HTML a
    eachRender ((chan, modu), v) =
      div
        []
        [ text ("chan: " <> chan <> ", module: " <> modu)
        , pre [] [text v]
        , pre [] [text "-----------"]
        ]

webServer :: TVar (Int, Inbox) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" (\_ -> go initVal)
  where
    go (i, m) = do
      let widget
            | i == 0 = pre [] [text "No GHC process yet"]
            | otherwise = div [] [text (T.pack (show i)), renderInbox m]
          await = liftSTM $ do
            (i', m') <- readTVar var
            if i == i'
              then retry
              else pure (i', m')
      val <- (widget <|> await)
      go val
