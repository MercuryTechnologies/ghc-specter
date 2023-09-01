{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.SourceView
  ( render,
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    readTVar,
  )
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text (Text)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Data.GHC.Hie (ModuleHieInfo (..))
import GHCSpecter.Graphics.DSL (Scene (..), Stage (..))
import GHCSpecter.Server.Types
  ( HieState (..),
    ServerState (..),
  )
import GHCSpecter.UI.Components.ModuleTree (buildModuleTree)
import GHCSpecter.UI.Components.TextView (buildTextView)
import GHCSpecter.UI.SourceView (buildSuppViewPanel)
import GHCSpecter.UI.Types
  ( SourceViewUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event (UserEvent (..))
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import ImGui qualified
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI
  ( defTableFlags,
    windowFlagsNoScroll,
    windowFlagsNoScrollbar,
  )
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  vec1 <- liftIO $ ImGui.newImVec2 400 0
  vec2 <- liftIO $ ImGui.newImVec2 400 0
  vec3 <- liftIO $ ImGui.newImVec2 400 0
  whenM (toBool <$> liftIO (ImGui.beginTable ("##table" :: CString) 3 defTableFlags)) $ do
    liftIO $ ImGui.tableSetupColumn_ ("graph" :: CString)
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    _ <- liftIO $ ImGui.beginChild ("#module-tree" :: CString) vec1 (fromBool False) windowFlagsNoScrollbar
    renderModuleTree srcUI ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 1
    _ <- liftIO $ ImGui.beginChild ("#source-view" :: CString) vec2 (fromBool False) windowFlagsNoScroll
    for_ mexpandedModu $ \modu ->
      renderSourceTextView modu ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 2
    _ <- liftIO $ ImGui.beginChild ("#supp-view" :: CString) vec3 (fromBool False) windowFlagsNoScroll
    for_ mexpandedModu $ \modu ->
      renderSuppViewPanel modu srcUI ss
    liftIO ImGui.endChild
  --
  liftIO ImGui.endTable
  liftIO $ delete vec1
  liftIO $ delete vec2
  liftIO $ delete vec3
  where
    srcUI = ui._uiModel._modelSourceView
    mexpandedModu = srcUI._srcViewExpandedModule

renderModuleTree :: SourceViewUI -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderModuleTree srcUI ss = do
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent
      False
      SourceViewEv
      (buildModuleTree srcUI ss)

renderSourceTextView :: Text -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSourceTextView modu ss = do
  let mmodHieInfo = M.lookup modu (hie._hieModuleMap)
  for_ mmodHieInfo $ \modHieInfo -> do
    let topLevelDecls = getReducedTopLevelDecls modHieInfo
        src = modHieInfo._modHieSource
    renderState <- mkRenderState
    let stage_ref :: TVar Stage
        stage_ref = renderState.currSharedState.sharedStage
    Stage stage <- liftIO $ atomically $ readTVar stage_ref
    for_ (L.find ((== "source-view") . sceneId) stage) $ \stage_source ->
      runImRender renderState $
        renderComponent
          True
          SourceViewEv
          ( do
              scene <- buildTextView src (fmap fst topLevelDecls)
              pure
                scene
                  { sceneId = "source-view",
                    sceneGlobalViewPort = stage_source.sceneGlobalViewPort,
                    sceneLocalViewPort = stage_source.sceneLocalViewPort
                  }
          )
  where
    hie = ss._serverHieState

renderSuppViewPanel :: Text -> SourceViewUI -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSuppViewPanel modu srcUI ss = do
  renderState <- mkRenderState
  let stage_ref :: TVar Stage
      stage_ref = renderState.currSharedState.sharedStage
  Stage stage <- liftIO $ atomically $ readTVar stage_ref
  for_ (L.find ((== "supple-view-tab") . sceneId) stage) $ \stage_supp_tab ->
    for_ (L.find ((== "supple-view-contents") . sceneId) stage) $ \stage_supp -> do
      runImRender renderState $ do
        let (sceneSuppTab, sceneSuppContents) =
              runIdentity (buildSuppViewPanel modu srcUI ss)
        renderComponent
          False
          SourceViewEv
          ( pure
              sceneSuppTab
                { sceneGlobalViewPort = stage_supp_tab.sceneGlobalViewPort,
                  sceneLocalViewPort = stage_supp_tab.sceneLocalViewPort
                }
          )
        renderComponent
          True
          (\_ -> DummyEv)
          ( pure
              sceneSuppContents
                { sceneGlobalViewPort = stage_supp.sceneGlobalViewPort,
                  sceneLocalViewPort = stage_supp.sceneLocalViewPort
                }
          )
