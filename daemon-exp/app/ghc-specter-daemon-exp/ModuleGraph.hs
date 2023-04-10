module ModuleGraph (
  renderModuleGraph,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((.~), (^.))
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Graphics.DSL (
  Scene (..),
  ViewPort (..),
 )
import GHCSpecter.Render.Components.GraphView (compileModuleGraph)
import GHCSpecter.UI.Constants (modGraphHeight, modGraphWidth)
import GHCSpecter.UI.Types (
  HasModuleGraphUI (..),
  HasUIState (..),
  HasUIViewRaw (..),
  HasViewPortInfo (..),
  ModuleGraphUI,
  UIState,
 )
import GI.Cairo.Render qualified as R
import Renderer (addEventMap, renderScene)
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
  R.liftIO $ atomically $ modifyTVar' uiRef (uiViewRaw . uiRawEventMap .~ [])
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
      scene = compileModuleGraph nameMap valueFor grVisInfo (Nothing, mhover)
      vpi = mgrui ^. modGraphViewPort
      vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
      scene' =
        scene
          { sceneGlobalViewPort = ViewPort (0, 0) (modGraphWidth, modGraphHeight)
          , sceneLocalViewPort = vp
          }
  renderScene vb scene'
  R.liftIO $ addEventMap uiRef scene'
