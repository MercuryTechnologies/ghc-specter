{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.SourceView (renderSourceView) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens (at, (^.), (^?), _Just)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.List qualified as L
import GHCSpecter.Data.GHC.Hie (
  HasModuleHieInfo (..),
 )
import GHCSpecter.Graphics.DSL (
  Scene (..),
  Stage (..),
 )
import GHCSpecter.Gtk.Renderer (render)
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (vruleLeft)
import GHCSpecter.Server.Types (
  HasHieState (..),
  HasServerState (..),
  ServerState,
 )
import GHCSpecter.UI.Components.ModuleTree (buildModuleTree)
import GHCSpecter.UI.Components.TextView (buildTextView)
import GHCSpecter.UI.SourceView (buildSuppViewPanel)
import GHCSpecter.UI.Types (
  HasSourceViewUI (..),
  SourceViewUI,
 )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)

renderSourceView ::
  SourceViewUI ->
  ServerState ->
  GtkRender UserEvent ()
renderSourceView srcUI ss = do
  stageRef <- vbStage <$> ask
  Stage stage <- liftIO $ atomically $ readTVar stageRef
  -- module tree pane
  for_ (L.find ((== "module-tree") . sceneId) stage) $ \scene0 -> do
    sceneModTree <- fmap (fmap SourceViewEv) <$> buildModuleTree srcUI ss
    let sceneModTree' =
          sceneModTree
            { sceneId = "module-tree"
            , sceneGlobalViewPort = sceneGlobalViewPort scene0
            , sceneLocalViewPort = sceneLocalViewPort scene0
            }
    render sceneModTree'
  -- source text view
  for_ (L.find ((== "source-view") . sceneId) stage) $ \scene0 -> do
    for_ (L.find ((== "supple-view") . sceneId) stage) $ \scene1 -> do
      -- source text
      vruleLeft (sceneGlobalViewPort scene0)
      let hie = ss ^. serverHieState
          mexpandedModu = srcUI ^. srcViewExpandedModule
      for_ mexpandedModu $ \modu -> do
        let mmodHieInfo = hie ^? hieModuleMap . at modu . _Just
        for_ mmodHieInfo $ \modHieInfo -> do
          let topLevelDecls = getReducedTopLevelDecls modHieInfo
              src = modHieInfo ^. modHieSource
          sceneSrcView <- buildTextView src (fmap fst topLevelDecls)
          let sceneSrcView' =
                sceneSrcView
                  { sceneId = "source-view"
                  , sceneGlobalViewPort = sceneGlobalViewPort scene0
                  , sceneLocalViewPort = sceneLocalViewPort scene0
                  }
          render sceneSrcView'
        -- supplementary view
        vruleLeft (sceneGlobalViewPort scene1)
        for_
          ( (,)
              <$> L.find ((== "supple-view-tab") . sceneId) stage
              <*> L.find ((== "supple-view-contents") . sceneId) stage
          )
          $ \(scene2, scene3) -> do
            (sceneSuppTab, sceneSuppContents) <- buildSuppViewPanel modu srcUI ss
            let sceneSuppTab' =
                  fmap (fmap SourceViewEv) $
                    sceneSuppTab
                      { sceneGlobalViewPort = sceneGlobalViewPort scene2
                      }
                sceneSuppContents' =
                  fmap
                    (DummyEv <$)
                    sceneSuppContents
                      { sceneGlobalViewPort = sceneGlobalViewPort scene3
                      , sceneLocalViewPort = sceneLocalViewPort scene3
                      }
            render sceneSuppTab'
            render sceneSuppContents'
