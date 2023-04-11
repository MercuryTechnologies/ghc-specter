{-# LANGUAGE OverloadedStrings #-}

module ModuleGraph (
  renderModuleGraph,
) where

import Control.Concurrent.STM (TVar)
import Control.Lens ((^.))
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.Render.Components.GraphView (compileModuleGraph)
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasViewPortInfo (..),
  ModuleGraphUI,
  UIState,
 )
import GI.Cairo.Render qualified as R
import Renderer (addEventMap, renderScene, resetWidget)
import Types (ViewBackend (..))

renderModuleGraph ::
  TVar UIState ->
  ViewBackend ->
  ModuleGraphUI ->
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  R.Render ()
renderModuleGraph uiRef vb mgrui nameMap drvModMap timing clustering grVisInfo = do
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
      mhover = mgrui ^. modGraphUIHover
      vpi = mgrui ^. modGraphViewPort
      vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)

  for_ (Map.lookup "main" wcfg) $ \vpCvs -> do
    let scene = compileModuleGraph nameMap valueFor grVisInfo (Nothing, mhover)
        scene' =
          scene
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vp
            }
    renderScene vb scene'
    R.liftIO $ addEventMap uiRef scene'
