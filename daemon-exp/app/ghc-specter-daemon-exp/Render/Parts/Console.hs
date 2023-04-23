{-# LANGUAGE OverloadedStrings #-}

module Render.Parts.Console (
  renderConsole,
) where

import Control.Lens (to, (^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.Map qualified as Map
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
  ViewPort (..),
 )
import GHCSpecter.Render.Components.Console (
  buildConsoleHelp,
  buildConsoleMain,
  buildConsoleTab,
 )
import GHCSpecter.Server.Types (
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types (
  HasConsoleUI (..),
  HasUIModel (..),
  HasUIState (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (ConsoleEvent (..), Event (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
import GI.Cairo.Render qualified as R
import Render.Util.Rules (boxRules)
import Renderer (addEventMap, renderScene, setColor)
import Types (GtkRender, ViewBackend (..))

renderConsole :: UIState -> ServerState -> GtkRender Event ()
renderConsole ui ss = do
  wcfg <- (^. to vbWidgetConfig . wcfgTopLevel) <$> ask
  let pausedMap = ss ^. serverPaused
      consoleMap = ss ^. serverConsole
      mconsoleFocus = ui ^. uiModel . modelConsole . consoleFocus
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
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    let sceneTab = ConsoleEv <$> buildConsoleTab tabs mconsoleFocus
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneTab'
    addEventMap sceneTab'
  for_ (Map.lookup "console-main" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    let sceneMain = ConsoleEv <$> buildConsoleMain consoleMap mconsoleFocus
        sceneMain' =
          sceneMain
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneMain'
    addEventMap sceneMain'
  for_ (Map.lookup "console-help" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor HoneyDew
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    boxRules vpCvs
    let sceneHelp = ConsoleEv <$> buildConsoleHelp getHelp mconsoleFocus
        sceneHelp' =
          sceneHelp
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneHelp'
    addEventMap sceneHelp'
