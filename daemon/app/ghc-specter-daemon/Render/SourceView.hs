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
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as M
import Data.Text (Text)
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
    renderModuleTree srcUI ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 1
    _ <- liftIO $ ImGui.beginChild ("#process-info" :: CString) vec2 (fromBool False) windowFlagsScroll
    for_ mexpandedModu $ \modu ->
      renderSourceTextView modu ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 2
    _ <- liftIO $ ImGui.beginChild ("#rts-info" :: CString) vec3 (fromBool False) windowFlagsScroll
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
  liftIO $
    runImRender renderState $
      renderComponent SourceViewEv (buildModuleTree srcUI ss)

renderSourceTextView :: Text -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSourceTextView modu ss = do
  let mmodHieInfo = M.lookup modu (hie._hieModuleMap)
  for_ mmodHieInfo $ \modHieInfo -> do
    let topLevelDecls = getReducedTopLevelDecls modHieInfo
        src = modHieInfo._modHieSource
    renderState <- mkRenderState
    liftIO $
      runImRender renderState $
        renderComponent
          SourceViewEv
          ( do
              scene <- buildTextView src (fmap fst topLevelDecls)
              pure scene {sceneId = "source-view"}
          )
  where
    hie = ss._serverHieState

renderSuppViewPanel :: Text -> SourceViewUI -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSuppViewPanel modu srcUI ss = do
  renderState <- mkRenderState
  liftIO $
    runImRender renderState $ do
      let (sceneSuppTab, sceneSuppContents) = runIdentity (buildSuppViewPanel modu srcUI ss)
      renderComponent
        SourceViewEv
        (pure sceneSuppTab)
      renderComponent
        (\_ -> DummyEv)
        (pure sceneSuppContents)
