{-# LANGUAGE OverloadedStrings #-}

module SourceView (renderSourceView) where

import Control.Concurrent.STM (TVar)
import Data.Foldable (for_, traverse_)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Tree (drawTree)
import GHCSpecter.Graphics.DSL (Scene (..), ViewPort (..))
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
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
        scene = compileModuleTree srcUI ss
        scene' =
          scene
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    -- R.setSourceRGBA 0 0 0 1
    -- R.setLineWidth 1.0
    -- R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
    -- R.fill
    -- R.liftIO $
    renderScene vb scene'
    R.liftIO $ addEventMap uiRef scene'
