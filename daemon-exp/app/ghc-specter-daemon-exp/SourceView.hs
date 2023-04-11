{-# LANGUAGE OverloadedStrings #-}

module SourceView (renderSourceView) where

import Control.Concurrent.STM (TVar)
import Data.Foldable (for_)
import Data.Map qualified as Map
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Tab (..))
import GI.Cairo.Render qualified as R
import Renderer (
  addEventMap,
  renderScene,
  resetWidget,
 )
import Types (ViewBackend)

renderSourceView ::
  TVar UIState ->
  ViewBackend ->
  R.Render ()
renderSourceView uiRef vb = do
  wcfg <- R.liftIO $ resetWidget uiRef
  for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
    let sceneTab = compileTab TabSourceView
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            }
    renderScene vb sceneTab'
    R.liftIO $ addEventMap uiRef sceneTab
