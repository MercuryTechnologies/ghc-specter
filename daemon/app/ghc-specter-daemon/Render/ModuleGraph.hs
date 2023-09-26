{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Render.ModuleGraph
  ( render,
  )
where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Error.Util (note)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL
  ( Scene (..),
    Stage (..),
  )
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.GraphView qualified as GraphView
import GHCSpecter.UI.Types
  ( ModuleGraphUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( SubModuleEvent (..),
    UserEvent (..),
  )
import ImGui qualified
import ImGui.Enum (ImGuiTableFlags_ (..))
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Text.Printf (printf)
import Util.GUI (windowFlagsNoScroll)
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> StateT (SharedState UserEvent) IO ()
render ui ss = do
  zerovec <- liftIO $ ImGui.newImVec2 0 0
  minusvec <- liftIO $ ImGui.newImVec2 0 (-200)
  let flags =
        fromIntegral $
          fromEnum ImGuiTableFlags_BordersOuter
            .|. fromEnum ImGuiTableFlags_BordersV
            .|. fromEnum ImGuiTableFlags_RowBg
            .|. fromEnum ImGuiTableFlags_Resizable
            .|. fromEnum ImGuiTableFlags_Reorderable
  whenM (toBool <$> liftIO (ImGui.beginTable ("##table" :: CString) 1 flags)) $ do
    liftIO $ ImGui.tableSetupColumn_ ("graph" :: CString)
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    _ <- liftIO $ ImGui.beginChild ("#main-modgraph" :: CString) minusvec (fromBool True) windowFlagsNoScroll
    renderMainModuleGraph ui ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    _ <- liftIO $ ImGui.beginChild ("#sub-modgraph" :: CString) zerovec (fromBool True) windowFlagsNoScroll
    renderSubModuleGraph ui ss
    liftIO ImGui.endChild
    liftIO ImGui.endTable
  liftIO $ delete zerovec
  liftIO $ delete minusvec

renderMainModuleGraph :: UIState -> ServerState -> StateT (SharedState UserEvent) IO ()
renderMainModuleGraph ui ss = do
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let mgrui = ui._uiModel._modelMainModuleGraph
          mainModuleClicked = mgrui._modGraphUIClick
          mainModuleHovered = mgrui._modGraphUIHover
      renderState <- mkRenderState
      shared <- get
      let stage_ref = shared.sharedStage
      Stage stage <- liftIO $ atomically $ readTVar stage_ref
      for_ (L.find ((== "main-module-graph") . sceneId) stage) $ \stageMain -> do
        runImRender renderState $
          renderComponent
            True
            MainModuleEv
            ( do
                scene <-
                  GraphView.buildModuleGraph
                    nameMap
                    valueFor
                    grVisInfo
                    (mainModuleClicked, mainModuleHovered)
                pure
                  scene
                    { sceneGlobalViewPort = stageMain.sceneGlobalViewPort,
                      sceneLocalViewPort = stageMain.sceneLocalViewPort
                    }
            )
  where
    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    drvModMap = ss._serverDriverModuleMap
    mgs = ss._serverModuleGraphState
    clustering = mgs._mgsClustering
    timing = ss._serverTiming._tsTimingMap
    valueFor name =
      fromMaybe 0 $ do
        cluster <- L.lookup name clustering
        let nTot = length cluster
        if nTot == 0
          then Nothing
          else do
            let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                nCompiled = length compiled
            pure (fromIntegral nCompiled / fromIntegral nTot)

renderSubModuleGraph :: UIState -> ServerState -> StateT (SharedState UserEvent) IO ()
renderSubModuleGraph ui ss = do
  case esubgraph of
    Left _err -> pure ()
    Right subgraph -> do
      let valueForSub name
            | isModuleCompilationDone drvModMap timing name = 1
            | otherwise = 0
      renderState <- mkRenderState
      shared <- get
      let stage_ref = shared.sharedStage
      Stage stage <- liftIO $ atomically $ readTVar stage_ref
      for_ (L.find ((== "sub-module-graph") . sceneId) stage) $ \stageSub -> do
        runImRender renderState $
          renderComponent
            True
            (SubModuleEv . SubModuleGraphEv)
            ( do
                scene <-
                  GraphView.buildModuleGraph
                    nameMap
                    valueForSub
                    subgraph
                    (mainModuleClicked, subModuleHovered)
                -- TODO: this should be set up from buildModuleGraph
                pure
                  scene
                    { sceneId = "sub-module-graph",
                      sceneGlobalViewPort = stageSub.sceneGlobalViewPort,
                      sceneLocalViewPort = stageSub.sceneLocalViewPort
                    }
            )
  where
    mgrui = ui._uiModel._modelMainModuleGraph
    (detailLevel, sgrui) = ui._uiModel._modelSubModuleGraph

    mainModuleClicked = mgrui._modGraphUIClick
    subModuleHovered = sgrui._modGraphUIHover

    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    drvModMap = ss._serverDriverModuleMap
    mgs = ss._serverModuleGraphState
    subgraphs = mgs._mgsSubgraph

    timing = ss._serverTiming._tsTimingMap

    esubgraph :: Either String _
    esubgraph = do
      selected <-
        note "no module cluster is selected" mainModuleClicked
      subgraphsAtTheLevel <-
        note (printf "%s subgraph is not computed" (show detailLevel)) (L.lookup detailLevel subgraphs)
      subgraph <-
        note
          (printf "cannot find the subgraph for the module cluster %s" (T.unpack selected))
          (L.lookup selected subgraphsAtTheLevel)
      pure subgraph
