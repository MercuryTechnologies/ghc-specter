{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.SourceView
  ( render,
  )
where

import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Data.Map qualified as M
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Data.GHC.Hie (ModuleHieInfo (..))
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Server.Types
  ( HieState (..),
    ServerState (..),
  )
import GHCSpecter.UI.Components.ModuleTree (buildModuleTree)
import GHCSpecter.UI.Components.TextView (buildTextView)
-- import GHCSpecter.UI.SourceView (buildSuppViewPanel)
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
import Util.GUI (defTableFlags, windowFlagsScroll)
import Util.Render
  ( SharedState (..),
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
    _ <- liftIO $ ImGui.beginChild ("#session-info" :: CString) vec1 (fromBool False) windowFlagsScroll
    renderModuleTree ui ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 1
    _ <- liftIO $ ImGui.beginChild ("#process-info" :: CString) vec2 (fromBool False) windowFlagsScroll
    renderSourceTextView ui ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 2
    _ <- liftIO $ ImGui.beginChild ("#rts-info" :: CString) vec3 (fromBool False) windowFlagsScroll
    -- renderRtsPanel ss
    liftIO ImGui.endChild
    --
    liftIO ImGui.endTable
  liftIO $ delete vec1
  liftIO $ delete vec2
  liftIO $ delete vec3

renderModuleTree :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderModuleTree ui ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $
      renderComponent SourceViewEv (buildModuleTree srcUI ss)
  where
    srcUI = ui._uiModel._modelSourceView

renderSourceTextView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSourceTextView ui ss = do
  let hie = ss._serverHieState
      mexpandedModu = srcUI._srcViewExpandedModule
  for_ mexpandedModu $ \modu -> do
    let mmodHieInfo = M.lookup modu (hie._hieModuleMap)
    for_ mmodHieInfo $ \modHieInfo -> do
      let topLevelDecls = getReducedTopLevelDecls modHieInfo
          src = modHieInfo._modHieSource
      renderState <- mkRenderState
      liftIO $
        runImRender renderState $
          renderComponent
            SourceViewEv
            (do
                scene <- buildTextView src (fmap fst topLevelDecls)
                pure scene {sceneId = "source-view"}
            )
  where
    srcUI = ui._uiModel._modelSourceView

{-
      sceneSrcView <- buildTextView src (fmap fst topLevelDecls)
      let sceneSrcView' =
            sceneSrcView
              { sceneId = "source-view",
                sceneGlobalViewPort = sceneGlobalViewPort scene0,
                sceneLocalViewPort = sceneLocalViewPort scene0
              }
-}
