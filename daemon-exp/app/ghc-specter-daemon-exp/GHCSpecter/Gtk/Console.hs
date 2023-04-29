{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Console (
  renderConsole,
) where

import Control.Lens (to, (^.))
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
 )
import GHCSpecter.Gtk.Renderer (addEventMap, renderScene, setColor)
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
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types (
  HasConsoleUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasViewPortInfo (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (ConsoleEvent (..), Event (..))
import GHCSpecter.Util.Transformation (translateToOrigin)

renderConsole :: UIState -> ServerState -> GtkRender Event ()
renderConsole ui ss = do
  wcfg <- (^. to vbWidgetConfig . wcfgTopLevel) <$> ask
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
  for_ (Map.lookup "console-tab" wcfg) $ \vpCvs -> do
    boxFill Ivory vpCvs
    setColor Ivory
    let sceneTab = fmap (fmap ConsoleEv) $ buildConsoleTab tabs mconsoleFocus
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneTab'
    addEventMap sceneTab'
  for_ (Map.lookup "console-main" wcfg) $ \vpCvs -> do
    let vpi = ui ^. uiModel . modelConsole . consoleViewPort
        vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
    boxFill Ivory vpCvs
    let sceneMain = fmap (fmap ConsoleEv) $ buildConsoleMain consoleMap mconsoleFocus
        sceneMain' =
          sceneMain
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene sceneMain'
    addEventMap sceneMain'
  for_ (Map.lookup "console-help" wcfg) $ \vpCvs -> do
    boxFill HoneyDew vpCvs
    boxRules vpCvs
    sceneHelp <- fmap (fmap ConsoleEv) <$> buildConsoleHelp getHelp mconsoleFocus
    let sceneHelp' =
          sceneHelp
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneHelp'
    addEventMap sceneHelp'
  for_ (Map.lookup "console-input" wcfg) $ \vpCvs -> do
    boxFill HoneyDew vpCvs
    boxRules vpCvs
    let sceneInput = buildConsoleInput inputEntry
        sceneInput' =
          sceneInput
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneInput'
    addEventMap sceneInput'
