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
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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
import GHCSpecter.UI.SourceView (buildSuppView)
import GHCSpecter.UI.Types
  ( SourceViewUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( SourceViewEvent (..),
    UserEvent (..),
  )
import GHCSpecter.Worker.CallGraph (getReducedTopLevelDecls)
import Handler (sendToControl)
import ImGui qualified
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI
  ( defTableFlags,
    makeTabContents,
    windowFlagsNoScroll,
    windowFlagsNoScrollbar,
  )
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> StateT (SharedState UserEvent) IO ()
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

renderModuleTree :: SourceViewUI -> ServerState -> StateT (SharedState UserEvent) IO ()
renderModuleTree srcUI ss = do
  renderState <- mkRenderState
  runImRender renderState $
    renderComponent
      False
      SourceViewEv
      (buildModuleTree srcUI ss)

renderSourceTextView :: Text -> ServerState -> StateT (SharedState UserEvent) IO ()
renderSourceTextView modu ss = do
  let mmodHieInfo = M.lookup modu (hie._hieModuleMap)
  for_ mmodHieInfo $ \modHieInfo -> do
    let topLevelDecls = getReducedTopLevelDecls modHieInfo
        src = modHieInfo._modHieSource
    renderState <- mkRenderState
    shared <- get
    let Stage stage = shared.sharedStage
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

renderSuppViewPanel :: Text -> SourceViewUI -> ServerState -> StateT (SharedState UserEvent) IO ()
renderSuppViewPanel modu srcUI ss = do
  shared <- get
  whenM (toBool <$> liftIO (ImGui.beginTabBar ("#supp-view-tabbar" :: CString))) $ do
    let tab_contents =
          fmap
            ( \((t, i), tab_title) ->
                ( (t, i),
                  tab_title,
                  renderSuppViewContents modu srcUI ss
                )
            )
            suppViewTabs
    mselected <- makeTabContents Nothing tab_contents
    when (mtab /= mselected) $
      case mselected of
        Nothing -> pure ()
        Just selected ->
          liftIO $ sendToControl shared (SourceViewEv (SourceViewTab selected))
    liftIO ImGui.endTabBar
  where
    mtab = srcUI._srcViewSuppViewTab
    suppViews = fromMaybe [] (M.lookup modu (ss._serverSuppView))
    suppViewTabs = fmap (\((t, i), _) -> ((t, i), t <> ":" <> T.pack (show i))) suppViews

renderSuppViewContents :: Text -> SourceViewUI -> ServerState -> StateT (SharedState UserEvent) IO ()
renderSuppViewContents modu srcUI ss = do
  renderState <- mkRenderState
  shared <- get
  let Stage stage = shared.sharedStage
  for_ (L.find ((== "supple-view-contents") . sceneId) stage) $ \stage_supp -> do
    runImRender renderState $ do
      renderComponent
        True
        (\_ -> DummyEv)
        ( do
            sceneSuppContents <- buildSuppView msupp_view
            pure
              sceneSuppContents
                { sceneGlobalViewPort = stage_supp.sceneGlobalViewPort,
                  sceneLocalViewPort = stage_supp.sceneLocalViewPort
                }
        )
  where
    mtab = srcUI._srcViewSuppViewTab
    suppViews = fromMaybe [] (M.lookup modu (ss._serverSuppView))
    msupp_view = do
      tab <- mtab
      suppView <- L.lookup tab suppViews
      pure suppView
