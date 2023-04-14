{-# LANGUAGE OverloadedStrings #-}

module Render.ModuleGraph (
  renderModuleGraph,
) where

import Control.Concurrent.STM (TVar)
import Control.Error.Util (note)
import Control.Lens ((^.))
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
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Graphics.DSL (Scene (..), ViewPort (..))
import GHCSpecter.Render.Components.GraphView (compileModuleGraph)
import GHCSpecter.Render.Components.Tab (compileTab)
import GHCSpecter.Render.Tab (topLevelTab)
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasViewPortInfo (..),
  ModuleGraphUI,
  UIState,
 )
import GHCSpecter.UI.Types.Event (DetailLevel, Tab (..))
import GI.Cairo.Render qualified as R
import Renderer (addEventMap, renderScene, resetWidget)
import Text.Printf (printf)
import Types (ViewBackend (..))

-- TODO: tidy up the parameters
renderModuleGraph ::
  TVar UIState ->
  ViewBackend ->
  (ModuleGraphUI, (DetailLevel, ModuleGraphUI)) ->
  [(DetailLevel, [(ModuleName, GraphVisInfo)])] ->
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  R.Render ()
renderModuleGraph
  uiRef
  vb
  (mgrui, (detailLevel, sgrui))
  subgraphs
  nameMap
  drvModMap
  timing
  clustering
  grVisInfo = do
    wcfg <- R.liftIO $ resetWidget uiRef
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
    -- tab
    for_ (Map.lookup "tab" wcfg) $ \vpCvs -> do
      let sceneTab = compileTab topLevelTab TabModuleGraph
          sceneTab' =
            sceneTab
              { sceneGlobalViewPort = vpCvs
              }
      renderScene vb sceneTab'
      R.liftIO $ addEventMap uiRef sceneTab
    -- main module graph
    for_ (Map.lookup "main-module-graph" wcfg) $ \vpCvs -> do
      let sceneMain =
            compileModuleGraph nameMap valueFor grVisInfo (mainModuleClicked, mainModuleHovered)
          sceneMain' =
            sceneMain
              { sceneGlobalViewPort = vpCvs
              , sceneLocalViewPort = vpMain
              }
      renderScene vb sceneMain'
      R.liftIO $ addEventMap uiRef sceneMain'
    -- sub module graph
    for_ (Map.lookup "sub-module-graph" wcfg) $ \vpCvs -> do
      let ViewPort (cx0, cy0) (cx1, _cy1) = vpCvs
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
      case esubgraph of
        Left err -> R.liftIO $ putStrLn err
        Right subgraph -> do
          let valueForSub name
                | isModuleCompilationDone drvModMap timing name = 1
                | otherwise = 0
              sceneSub =
                compileModuleGraph nameMap valueForSub subgraph (mainModuleClicked, subModuleHovered)
              sceneSub' =
                sceneSub
                  { -- TODO: this should be set up from compileModuleGraph
                    sceneId = "sub-module-graph"
                  , sceneGlobalViewPort = vpCvs
                  , sceneLocalViewPort = vpSub
                  }
          -- separator rule
          R.setSourceRGBA 0 0 0 1
          R.setLineWidth 1.0
          R.moveTo cx0 cy0
          R.lineTo cx1 cy0
          R.stroke
          renderScene vb sceneSub'
          R.liftIO $ addEventMap uiRef sceneSub'
