{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Render.ModuleGraph
  ( renderMainModuleGraph,
    renderSubModuleGraph,
  )
where

import Control.Error.Util (note)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL
  ( Primitive,
    Scene (..),
    ViewPort (..),
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
import Handler
  ( handleClick,
    handleMove,
  )
import ImGui
import STD.Deletable (delete)
import Text.Printf (printf)
import Util.Render
  ( SharedState (..),
    addEventMap,
    buildEventMap,
    mkRenderState,
    renderScene,
    runImRender,
  )

renderMainModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMainModuleGraph ui ss = do
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let mgrui = ui._uiModel._modelMainModuleGraph

          mainModuleClicked = mgrui._modGraphUIClick
          mainModuleHovered = mgrui._modGraphUIHover

          scene :: Scene (Primitive UserEvent)
          scene =
            fmap MainModuleEv
              <$> ( runIdentity $
                      GraphView.buildModuleGraph nameMap valueFor grVisInfo (mainModuleClicked, mainModuleHovered)
                  )
          emap = buildEventMap scene
          (vx0, vy0) = scene.sceneLocalViewPort.topLeft
          (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
          totalW = vx1 - vx0
          totalH = vy1 - vy0
      renderState <- mkRenderState
      liftIO $ do
        runImRender renderState $ do
          renderScene scene
          addEventMap emap
          dummy_sz <- liftIO $ newImVec2 (realToFrac totalW) (realToFrac totalH)
          liftIO $ dummy dummy_sz
          -- handling event
          handleMove scene.sceneId
          handleClick scene.sceneId
          liftIO $ delete dummy_sz
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
          sceneSub :: Scene (Primitive UserEvent)
          sceneSub =
            fmap (SubModuleEv . SubModuleGraphEv)
              <$> ( runIdentity $
                      GraphView.buildModuleGraph nameMap valueForSub subgraph (mainModuleClicked, subModuleHovered)
                  )
          -- TODO: this should be set up from buildModuleGraph
          sceneSub' = sceneSub {sceneId = "sub-module-graph"}
          emap = buildEventMap sceneSub'
          (vx0, vy0) = sceneSub'.sceneLocalViewPort.topLeft
          (vx1, vy1) = sceneSub'.sceneLocalViewPort.bottomRight
          totalW = vx1 - vx0
          totalH = vy1 - vy0
      renderState <- mkRenderState
      liftIO $ do
        runImRender renderState $ do
          renderScene sceneSub'
          addEventMap emap
          dummy_sz <- liftIO $ newImVec2 (realToFrac totalW) (realToFrac totalH)
          liftIO $ dummy dummy_sz
          -- handling event
          handleMove sceneSub'.sceneId
          handleClick sceneSub'.sceneId
          liftIO $ delete dummy_sz
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
