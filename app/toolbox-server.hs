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

main :: IO ()
main = do
  var <- atomically $ newTVar (0, mempty)
  _ <- forkIO $ listener var
  webServer var

type ChanModule = (Text, Text)

type ModuleMessages = Map ChanModule Text

listener :: TVar (Int, ModuleMessages) -> IO ()
listener var =
  runServer
    "/tmp/ghc-build-analyzer.ipc"
    (\sock -> receiveObject sock >>= atomically . updateModuleMessages var)

updateModuleMessages :: TVar (Int, ModuleMessages) -> ChanMessageBox -> STM ()
updateModuleMessages var chanMsg =
  modifyTVar' var $ \(i, m) ->
    let (chan, modu, msg) =
          case chanMsg of
            CMBox (CMCheckImports m' t') -> ("check-imports", m', t')
            CMBox (CMTrivial m') -> ("trivial", m', "no-message")
     in (i + 1, M.insert (chan, modu) msg m)

renderModuleMessages :: ModuleMessages -> Widget HTML a
renderModuleMessages m =
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
