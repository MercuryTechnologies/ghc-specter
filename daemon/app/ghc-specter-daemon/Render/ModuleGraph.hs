{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Render.ModuleGraph
  ( render,
    renderBlockerGraph,
  )
where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Error.Util (note)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Bits ((.|.))
import Data.Foldable (for_, traverse_)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time.Clock (secondsToNominalDiffTime)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Map (backwardLookup)
import GHCSpecter.Data.Timing.Types (PipelineInfo (..), TimingTable (..))
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
    TimingUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( BlockerDetailLevel (..),
    BlockerEvent (..),
    BlockerModuleGraphEvent (..),
    SubModuleEvent (..),
    TimingEvent (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui qualified
import ImGui.Enum (ImGuiTableFlags_ (..))
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Text.Printf (printf)
import Util.GUI (windowFlagsNoScroll)
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
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

renderMainModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMainModuleGraph ui ss = do
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let mgrui = ui._uiModel._modelMainModuleGraph
          mainModuleClicked = mgrui._modGraphUIClick
          mainModuleHovered = mgrui._modGraphUIHover
      renderState <- mkRenderState
      let stage_ref = renderState.currSharedState.sharedStage
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

renderSubModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSubModuleGraph ui ss = do
  case esubgraph of
    Left _err -> pure ()
    Right subgraph -> do
      let valueForSub name
            | isModuleCompilationDone drvModMap timing name = 1
            | otherwise = 0
      renderState <- mkRenderState
      let stage_ref = renderState.currSharedState.sharedStage
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

renderBlockerGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderBlockerGraph ui ss = do
  shared <- ask
  liftIO $ do
    let opts :: [(CString, BlockerDetailLevel)]
        opts =
          [ (">=2", Blocking2),
            (">=3", Blocking3),
            (">=4", Blocking4),
            (">=5", Blocking5)
          ]
        mkRadio (label, level) = do
          whenM (toBool <$> ImGui.radioButton_ label (fromBool (curr_level == level))) $
            sendToControl shared (BlockerEv (BlockerModuleGraphEv (BMGUpdateLevel level)))
          ImGui.sameLine_
    traverse_ mkRadio opts
    whenM (toBool <$> ImGui.button ("Re-compute Blocker Graph" :: CString)) $ do
      sendToControl shared (BlockerEv ComputeBlockerGraph)
  case mblockerGraphViz of
    Nothing -> pure ()
    Just blockerGraphViz -> do
      renderState <- mkRenderState
      let stage_ref = renderState.currSharedState.sharedStage
      Stage stage <- liftIO $ atomically $ readTVar stage_ref
      for_ (L.find ((== "blocker-module-graph") . sceneId) stage) $ \stageBlocker -> do
        runImRender renderState $
          renderComponent
            True
            (BlockerEv . BlockerModuleGraphEv . BMGGraph)
            ( do
                scene <- GraphView.buildModuleGraph nameMap valueFor blockerGraphViz (Nothing, Nothing)
                -- TODO: this should be set up from buildModuleGraph
                pure
                  scene
                    { sceneId = "blocker-module-graph",
                      sceneGlobalViewPort = stageBlocker.sceneGlobalViewPort,
                      sceneLocalViewPort = stageBlocker.sceneLocalViewPort
                    }
            )
  where
    drvModMap = ss._serverDriverModuleMap
    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    ttable = ss._serverTiming._tsTimingTable
    curr_level = ss._serverTiming._tsBlockerDetailLevel
    maxTime =
      case ttable._ttableTimingInfos of
        [] -> secondsToNominalDiffTime 1.0
        ts -> maximum (fmap (\(_, t) -> fst t._plEnd - fst t._plStart) ts)
    mblockerGraphViz = ss._serverTiming._tsBlockerGraphViz
    valueFor name =
      fromMaybe 0 $ do
        i <- backwardLookup name drvModMap
        t <- L.lookup i (ttable._ttableTimingInfos)
        pure $ realToFrac ((fst t._plEnd - fst t._plStart) / maxTime)
