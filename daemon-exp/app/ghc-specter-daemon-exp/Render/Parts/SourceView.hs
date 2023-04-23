{-# LANGUAGE OverloadedStrings #-}

module Render.Parts.SourceView (renderSourceView) where

import Control.Lens (at, to, (^.), (^?), _Just)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHCSpecter.Data.GHC.Hie (
  HasModuleHieInfo (..),
 )
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.ModuleTree (buildModuleTree)
import GHCSpecter.Render.Components.TextView (buildTextView)
import GHCSpecter.Render.SourceView (buildSuppViewPanel)
import GHCSpecter.Server.Types (
  HasHieState (..),
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Types (
  HasSourceViewUI (..),
  HasViewPortInfo (..),
  SourceViewUI,
 )
import GHCSpecter.UI.Types.Event (Event (..))
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import Render.Util.Rules (vruleLeft)
import Renderer (
  addEventMap,
  renderScene,
 )
import Types (GtkRender, ViewBackend (..))

renderSourceView ::
  SourceViewUI ->
  ServerState ->
  GtkRender Event ()
renderSourceView srcUI ss = do
  wcfg <- (^. to vbWidgetConfig . wcfgSourceView) <$> ask
  -- module tree pane
  for_ (Map.lookup "module-tree" wcfg) $ \vpCvs -> do
    let vp = srcUI ^. srcViewModuleTreeViewPort . vpViewPort
        sceneModTree = SourceViewEv <$> buildModuleTree srcUI ss
        sceneModTree' =
          sceneModTree
            { sceneId = "module-tree"
            , sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene sceneModTree'
    addEventMap sceneModTree'
  -- source text view
  for_ (Map.lookup "source-view" wcfg) $ \vpCvs -> do
    for_ (Map.lookup "supple-view" wcfg) $ \vpCvsSupp -> do
      -- source text
      vruleLeft vpCvs
      let hie = ss ^. serverHieState
          mexpandedModu = srcUI ^. srcViewExpandedModule
      for_ mexpandedModu $ \modu -> do
        let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
        for_ mmodHieInfo $ \modHieInfo -> do
          let topLevelDecls = getReducedTopLevelDecls modHieInfo
              src = modHieInfo ^. modHieSource
              vpi = srcUI ^. srcViewSourceViewPort
              vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
              sceneSrcView = buildTextView src (fmap fst topLevelDecls)
              sceneSrcView' =
                sceneSrcView
                  { sceneId = "source-view"
                  , sceneGlobalViewPort = vpCvs
                  , sceneLocalViewPort = vp
                  }
          renderScene sceneSrcView'
          addEventMap sceneSrcView'
        -- supplementary view
        vruleLeft vpCvsSupp
        for_ ((,) <$> Map.lookup "supple-view-tab" wcfg <*> Map.lookup "supple-view-contents" wcfg) $
          \(vpCvsSuppTab, vpCvsSuppContents) -> do
            let vpi = srcUI ^. srcViewSuppViewPort
                vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
                (sceneSuppTab, sceneSuppContents) = buildSuppViewPanel modu srcUI ss
                sceneSuppTab' =
                  SourceViewEv
                    <$> sceneSuppTab
                      { sceneGlobalViewPort = vpCvsSuppTab
                      }
                sceneSuppContents' =
                  DummyEv
                    <$ sceneSuppContents
                      { sceneGlobalViewPort = vpCvsSuppContents
                      , sceneLocalViewPort = vp
                      }
            renderScene sceneSuppTab'
            addEventMap sceneSuppTab'
            renderScene sceneSuppContents'
            addEventMap sceneSuppContents'
