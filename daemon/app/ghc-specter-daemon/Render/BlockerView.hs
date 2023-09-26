{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.BlockerView
  ( render,
  )
where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (for_, traverse_)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Time.Clock (secondsToNominalDiffTime)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Map (backwardLookup)
import GHCSpecter.Data.Timing.Types (PipelineInfo (..), TimingTable (..))
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
import GHCSpecter.UI.Types (UIState (..))
import GHCSpecter.UI.Types.Event
  ( BlockerDetailLevel (..),
    BlockerEvent (..),
    BlockerModuleGraphEvent (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui qualified
import Render.Common (renderComponent)
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render _ui ss = do
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
      let stage_ref = shared.sharedStage
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
