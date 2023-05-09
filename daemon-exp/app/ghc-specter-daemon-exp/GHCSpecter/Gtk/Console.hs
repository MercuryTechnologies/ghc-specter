{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Console (
  renderConsole,
) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Map (
  forwardLookup,
  keyMapToList,
  lookupKey,
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  Scene (..),
  Stage (..),
 )
import GHCSpecter.Gtk.Renderer (render, setColor)
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (boxFill, boxRules)
import GHCSpecter.Server.Types (
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Console (
  buildConsoleHelp,
  buildConsoleInput,
  buildConsoleMain,
  buildConsoleTab,
 )
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types (
  HasConsoleUI (..),
  HasUIModel (..),
  HasUIState (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (ConsoleEvent (..), UserEvent (..))

renderConsole :: UIState -> ServerState -> GtkRender UserEvent ()
renderConsole ui ss = do
  stageRef <- vbStage <$> ask
  Stage stage <- liftIO $ atomically $ readTVar stageRef
  let pausedMap = ss ^. serverPaused
      consoleMap = ss ^. serverConsole
      mconsoleFocus = ui ^. uiModel . modelConsole . consoleFocus
      inputEntry = ui ^. uiModel ^. modelConsole . consoleInputEntry
      -- TODO: refactor this out and this should be out of Render.*
      getTabName k =
        let ktxt = T.pack $ show (unDriverId k)
            mlookedup = forwardLookup k (ss ^. serverDriverModuleMap)
         in maybe ktxt (\m -> ktxt <> " - " <> m) mlookedup
      tabs = fmap (\(k, _) -> (k, getTabName k)) . keyMapToList $ pausedMap
      getHelp k =
        let title =
              let mpaused = lookupKey k pausedMap
               in maybe "" (\loc -> "paused at " <> T.pack (show loc)) mpaused
            classify txt =
              if txt == ":next" || txt == ":goto-source" || txt == ":dump-heap" || txt == ":exit-ghc-debug" || txt == ":list-core"
                then Left (txt, ConsoleButtonPressed True txt)
                else Right txt
            helpMsgs =
              maybe
                [Right "No Help!"]
                (fmap classify)
                (consoleCommandList <$> lookupKey k (ss ^. serverPaused))
         in (title, helpMsgs)
  for_ (L.find ((== "console-tab") . sceneId) stage) $ \scene0 -> do
    boxFill Ivory (sceneGlobalViewPort scene0)
    setColor Ivory
    sceneTab <- fmap (fmap ConsoleEv) <$> buildConsoleTab tabs mconsoleFocus
    let sceneTab' =
          sceneTab
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneTab'
  for_ (L.find ((== "console-main") . sceneId) stage) $ \scene0 -> do
    boxFill Ivory (sceneGlobalViewPort scene0)
    sceneMain <- fmap (fmap ConsoleEv) <$> buildConsoleMain consoleMap mconsoleFocus
    let sceneMain' =
          sceneMain
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneMain'
  for_ (L.find ((== "console-help") . sceneId) stage) $ \scene0 -> do
    boxFill HoneyDew (sceneGlobalViewPort scene0)
    boxRules (sceneGlobalViewPort scene0)
    sceneHelp <- fmap (fmap ConsoleEv) <$> buildConsoleHelp getHelp mconsoleFocus
    let sceneHelp' =
          sceneHelp
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneHelp'
  for_ (L.find ((== "console-input") . sceneId) stage) $ \scene0 -> do
    boxFill HoneyDew (sceneGlobalViewPort scene0)
    boxRules (sceneGlobalViewPort scene0)
    sceneInput <- buildConsoleInput inputEntry
    let sceneInput' =
          sceneInput
            { sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneInput'
