{-# LANGUAGE OverloadedStrings #-}

module SourceView (renderSourceView) where

import Control.Concurrent.STM (TVar)
import Control.Lens (at, (^.), (^?), _Just)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import GHCSpecter.Data.GHC.Hie (
  HasDeclRow' (..),
  HasModuleHieInfo (..),
  ModuleHieInfo,
 )
import GHCSpecter.Graphics.DSL (Scene (..), ViewPort (..))
import GHCSpecter.Render.Components.ModuleTree (compileModuleTree)
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Render.Components.TextView (compileTextView)
import GHCSpecter.Server.Types (
  HasHieState (..),
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Types (
  HasSourceViewUI (..),
  SourceViewUI,
  UIState,
 )
import GHCSpecter.UI.Types.Event (Tab (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
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
  -- module tree pane
  for_ (Map.lookup "module-tree" wcfg) $ \vpCvs -> do
    let sceneModTree = compileModuleTree srcUI ss
        sceneModTree' =
          sceneModTree
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    renderScene vb sceneModTree'
    R.liftIO $ addEventMap uiRef sceneModTree'
  -- source text view
  for_ (Map.lookup "source-view" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (_cx1, cy1) = vpCvs
    -- vertical separator
    R.setSourceRGBA 0 0 0 1
    R.setLineWidth 1.0
    R.moveTo cx0 cy0
    R.lineTo cx0 cy1
    R.stroke
    -- source text
    let hie = ss ^. serverHieState
        mexpandedModu = srcUI ^. srcViewExpandedModule
    for_ mexpandedModu $ \modu -> do
      let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
      for_ mmodHieInfo $ \modHieInfo -> do
        let topLevelDecls = getReducedTopLevelDecls modHieInfo
            src = modHieInfo ^. modHieSource
        let sceneSrcView = compileTextView False src []
            sceneSrcView' =
              sceneSrcView
                { sceneGlobalViewPort = vpCvs
                , sceneLocalViewPort = translateToOrigin vpCvs
                }
        renderScene vb sceneSrcView'
        R.liftIO $ addEventMap uiRef sceneSrcView'
