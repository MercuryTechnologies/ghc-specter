{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.ModuleGraph
  ( renderModuleGraph,
  )
where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Error.Util (note)
import Control.Lens ((^.))
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL
  ( Scene (..),
    Stage (..),
  )
import GHCSpecter.Gtk.Renderer (render)
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (hruleTop)
import GHCSpecter.Layouter.Graph.Types (GraphVisInfo)
import GHCSpecter.UI.Components.GraphView (buildModuleGraph)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    ModuleGraphUI,
  )
import GHCSpecter.UI.Types.Event
  ( DetailLevel,
    SubModuleEvent (..),
    UserEvent (..),
  )
import GI.Cairo.Render qualified as R
import Text.Printf (printf)

-- TODO: tidy up the parameters
renderModuleGraph ::
  (ModuleGraphUI, (DetailLevel, ModuleGraphUI)) ->
  [(DetailLevel, [(ModuleName, GraphVisInfo)])] ->
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  GtkRender UserEvent ()
renderModuleGraph
  (mgrui, (detailLevel, sgrui))
  subgraphs
  nameMap
  drvModMap
  timing
  clustering
  grVisInfo = do
    stageRef <- vbStage <$> ask
    Stage stage <- R.liftIO $ atomically $ readTVar stageRef
    let valueFor name =
          fromMaybe 0 $ do
            cluster <- L.lookup name clustering
            let nTot = length cluster
            if nTot == 0
              then Nothing
              else do
                let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                    nCompiled = length compiled
                pure (fromIntegral nCompiled / fromIntegral nTot)
        mainModuleClicked = mgrui ^. modGraphUIClick
        mainModuleHovered = mgrui ^. modGraphUIHover
        subModuleHovered = sgrui ^. modGraphUIHover
    -- main module graph
    for_ (L.find ((== "main-module-graph") . sceneId) stage) $ \scene0 -> do
      sceneMain <-
        fmap (fmap MainModuleEv)
          <$> buildModuleGraph
            nameMap
            valueFor
            grVisInfo
            (mainModuleClicked, mainModuleHovered)
      let sceneMain' =
            sceneMain
              { sceneGlobalViewPort = sceneGlobalViewPort scene0,
                sceneLocalViewPort = sceneLocalViewPort scene0
              }
      render sceneMain'
    -- sub module graph
    for_ (L.find ((== "sub-module-graph") . sceneId) stage) $ \scene0 -> do
      let esubgraph = do
            selected <-
              note "no module cluster is selected" mainModuleClicked
            subgraphsAtTheLevel <-
              note (printf "%s subgraph is not computed" (show detailLevel)) (L.lookup detailLevel subgraphs)
            subgraph <-
              note
                (printf "cannot find the subgraph for the module cluster %s" (T.unpack selected))
                (L.lookup selected subgraphsAtTheLevel)
            pure subgraph
      case esubgraph of
        Left err -> R.liftIO $ putStrLn err
        Right subgraph -> do
          let valueForSub name
                | isModuleCompilationDone drvModMap timing name = 1
                | otherwise = 0
          sceneSub <-
            fmap (fmap (SubModuleEv . SubModuleGraphEv))
              <$> buildModuleGraph
                nameMap
                valueForSub
                subgraph
                (mainModuleClicked, subModuleHovered)
          let sceneSub' =
                sceneSub
                  { -- TODO: this should be set up from buildModuleGraph
                    sceneId = "sub-module-graph",
                    sceneGlobalViewPort = sceneGlobalViewPort scene0,
                    sceneLocalViewPort = sceneLocalViewPort scene0
                  }
          -- separator rule
          hruleTop (sceneGlobalViewPort scene0)
          render sceneSub'
