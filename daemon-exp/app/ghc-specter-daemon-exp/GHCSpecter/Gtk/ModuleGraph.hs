{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.ModuleGraph (
  renderModuleGraph,
) where

import Control.Error.Util (note)
import Control.Lens (to, (^.))
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Gtk.Renderer (render)
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (hruleTop)
import GHCSpecter.Layouter.Graph.Types (GraphVisInfo)
import GHCSpecter.UI.Components.GraphView (buildModuleGraph)
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasViewPortInfo (..),
  ModuleGraphUI,
 )
import GHCSpecter.UI.Types.Event (
  DetailLevel,
  Event (..),
  SubModuleEvent (..),
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
  GtkRender Event ()
renderModuleGraph
  (mgrui, (detailLevel, sgrui))
  subgraphs
  nameMap
  drvModMap
  timing
  clustering
  grVisInfo = do
    wcfg <- (^. to vbWidgetConfig . wcfgModuleGraph) <$> ask
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
        vpiMain = mgrui ^. modGraphViewPort
        vpMain = fromMaybe (vpiMain ^. vpViewPort) (vpiMain ^. vpTempViewPort)
        vpiSub = sgrui ^. modGraphViewPort
        vpSub = fromMaybe (vpiSub ^. vpViewPort) (vpiSub ^. vpTempViewPort)
    -- main module graph
    for_ (Map.lookup "main-module-graph" wcfg) $ \vpCvs -> do
      sceneMain <-
        fmap (fmap MainModuleEv)
          <$> buildModuleGraph
            nameMap
            valueFor
            grVisInfo
            (mainModuleClicked, mainModuleHovered)
      let sceneMain' =
            sceneMain
              { sceneGlobalViewPort = vpCvs
              , sceneLocalViewPort = vpMain
              }
      render sceneMain'
    -- sub module graph
    for_ (Map.lookup "sub-module-graph" wcfg) $ \vpCvs -> do
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
                    sceneId = "sub-module-graph"
                  , sceneGlobalViewPort = vpCvs
                  , sceneLocalViewPort = vpSub
                  }
          -- separator rule
          hruleTop vpCvs
          render sceneSub'
