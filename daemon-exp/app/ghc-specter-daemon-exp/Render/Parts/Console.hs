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
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  Scene (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.Console (compileConsoleTab)
import GHCSpecter.Server.Types (
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Types (
  HasConsoleUI (..),
  HasUIModel (..),
  HasUIState (..),
  UIState,
 )
import GHCSpecter.UI.Types.Event (Event (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
import GI.Cairo.Render qualified as R
import Render.Util.Rules (boxRules)
import Renderer (addEventMap, setColor, renderScene)
import Types (GtkRender, ViewBackend (..))

renderConsole :: UIState -> ServerState -> GtkRender Event ()
renderConsole ui ss = do
  wcfg <- (^. to vbWidgetConfig . wcfgTopLevel) <$> ask
  for_ (Map.lookup "console-tab" wcfg) $ \vpCvs -> do
    let pausedMap = ss ^. serverPaused
        mconsoleFocus = ui ^. uiModel . modelConsole . consoleFocus
        -- TODO: refactor this out
        getTabName k =
          let ktxt = T.pack $ show (unDriverId k)
              mlookedup = forwardLookup k (ss ^. serverDriverModuleMap)
           in maybe ktxt (\m -> ktxt <> " - " <> m) mlookedup
        tabs = fmap (\(k, _) -> (k, getTabName k)) . keyMapToList $ pausedMap
        sceneTab = ConsoleEv <$> compileConsoleTab tabs mconsoleFocus
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene sceneTab'
    addEventMap sceneTab'
{-    
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    boxRules vpCvs  -}
  for_ (Map.lookup "console-main" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    boxRules vpCvs
