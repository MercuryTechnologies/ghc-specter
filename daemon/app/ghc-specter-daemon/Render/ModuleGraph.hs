{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Render.ModuleGraph
  ( render,
    renderBlockerGraph,
  )
where

import Control.Error.Util (note)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Bits ((.|.))
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
import GHCSpecter.Graphics.DSL (Scene (..))
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
  ( BlockerModuleGraphEvent (..),
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
import Util.GUI (windowFlagsScroll)
import Util.Render
  ( SharedState (..),
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
    _ <- liftIO $ ImGui.beginChild ("#main-modgraph" :: CString) minusvec (fromBool False) windowFlagsScroll
    renderMainModuleGraph ui ss
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    _ <- liftIO $ ImGui.beginChild ("#sub-modgraph" :: CString) zerovec (fromBool False) windowFlagsScroll
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
      liftIO $
        runImRender renderState $
          renderComponent
            MainModuleEv
            (GraphView.buildModuleGraph nameMap valueFor grVisInfo (mainModuleClicked, mainModuleHovered))
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
      liftIO $
        runImRender renderState $
          renderComponent
            (SubModuleEv . SubModuleGraphEv)
            ( do
                scene <- GraphView.buildModuleGraph nameMap valueForSub subgraph (mainModuleClicked, subModuleHovered)
                -- TODO: this should be set up from buildModuleGraph
                pure scene {sceneId = "sub-module-graph"}
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
renderBlockerGraph _ui ss = do
  whenM (toBool <$> liftIO (ImGui.button ("Re-compute Blocker Graph" :: CString))) $ do
    shared <- ask
    liftIO $ sendToControl shared (TimingEv ShowBlockerGraph)
  case mblockerGraphViz of
    Nothing -> pure ()
    Just blockerGraphViz -> do
      renderState <- mkRenderState
      liftIO $
        runImRender renderState $
          renderComponent
            (TimingEv . BlockerModuleGraphEv . BMGGraph)
            ( do
                scene <- GraphView.buildModuleGraph nameMap valueFor blockerGraphViz (Nothing, Nothing)
                -- TODO: this should be set up from buildModuleGraph
                pure scene {sceneId = "blocker-module-graph"}
            )
  where
    drvModMap = ss._serverDriverModuleMap
    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    ttable = ss._serverTiming._tsTimingTable
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
