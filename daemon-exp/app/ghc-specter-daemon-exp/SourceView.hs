{-# LANGUAGE OverloadedStrings #-}

module SourceView (renderSourceView) where

import Control.Concurrent.STM (TVar)
import Data.Foldable (for_)
import Data.Map qualified as Map
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.ModuleTree (compileModuleTree)
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types (
  SourceViewUI,
  UIState,
 )
import GHCSpecter.UI.Types.Event (Tab (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
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
  SourceViewUI ->
  ServerState ->
  R.Render ()
renderSourceView uiRef vb srcUI ss = do
  wcfg <- R.liftIO $ resetWidget uiRef
  for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
    let sceneTab = compileTab TabSourceView
        sceneTab' =
          sceneTab
            { sceneGlobalViewPort = vpCvs
            }
    renderScene vb sceneTab'
    R.liftIO $ addEventMap uiRef sceneTab
  for_ (Map.lookup "module-tree" wcfg) $ \vpCvs -> do
    let scene = compileModuleTree srcUI ss
        scene' =
          scene
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene vb scene'
    R.liftIO $ addEventMap uiRef scene'
