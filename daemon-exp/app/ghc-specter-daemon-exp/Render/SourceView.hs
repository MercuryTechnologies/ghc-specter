{-# LANGUAGE OverloadedStrings #-}

module Render.SourceView (renderSourceView) where

import Control.Concurrent.STM (TVar)
import Control.Lens (at, (^.), (^?), _Just)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHCSpecter.Data.GHC.Hie (
  HasModuleHieInfo (..),
 )
import GHCSpecter.Graphics.DSL (Scene (..))
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
  HasViewPortInfo (..),
  SourceViewUI,
  UIState,
 )
import GHCSpecter.UI.Types.Event (Tab (..))
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import GI.Cairo.Render qualified as R
import Render.Common (vruleLeft)
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
    let vp = srcUI ^. srcViewModuleTreeViewPort . vpViewPort
        sceneModTree = compileModuleTree srcUI ss
        sceneModTree' =
          sceneModTree
            { sceneId = "module-tree"
            , sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene vb sceneModTree'
    R.liftIO $ addEventMap uiRef sceneModTree'
  -- source text view
  for_ (Map.lookup "source-view" wcfg) $ \vpCvs -> do
    vruleLeft vpCvs
    -- source text
    let hie = ss ^. serverHieState
        mexpandedModu = srcUI ^. srcViewExpandedModule
    for_ mexpandedModu $ \modu -> do
      let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
      for_ mmodHieInfo $ \modHieInfo -> do
        let topLevelDecls = getReducedTopLevelDecls modHieInfo
            src = modHieInfo ^. modHieSource
            vpi = srcUI ^. srcViewSourceViewPort
            vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
            sceneSrcView = compileTextView src (fmap fst topLevelDecls)
            sceneSrcView' =
              sceneSrcView
                { sceneId = "source-view"
                , sceneGlobalViewPort = vpCvs
                , sceneLocalViewPort = vp
                }
        renderScene vb sceneSrcView'
        R.liftIO $ addEventMap uiRef sceneSrcView'
  -- supplementary view
  for_ (Map.lookup "supple-view" wcfg) $ \vpCvs -> do
    vruleLeft vpCvs
