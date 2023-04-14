{-# LANGUAGE OverloadedStrings #-}

module Render.Session (renderSession) where

import Control.Concurrent.STM (TVar)
import Data.Foldable (for_)
import Data.Map qualified as Map
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Render.Tab (topLevelTab)
import GHCSpecter.UI.Types (UIState)
import GHCSpecter.UI.Types.Event (Tab (..))
import GI.Cairo.Render qualified as R
import Renderer (
  addEventMap,
  renderScene,
  resetWidget,
 )
import Types (ViewBackend)

renderSession ::
  TVar UIState ->
  ViewBackend ->
  R.Render ()
renderSession uiRef vb = do
  wcfg <- R.liftIO $ resetWidget uiRef
  for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
    let sceneTab = compileTab topLevelTab TabSession
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            }
    renderScene vb sceneTab'
    R.liftIO $ addEventMap uiRef sceneTab
