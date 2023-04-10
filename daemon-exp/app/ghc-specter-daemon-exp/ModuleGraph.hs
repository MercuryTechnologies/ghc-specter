module ModuleGraph (
  renderModuleGraph,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens ((.~), (^.))
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCSpecter.Channel.Common.Types (DriverId, ModuleName)
import GHCSpecter.Channel.Outbound.Types (Timer)
import GHCSpecter.Data.Map (BiKeyMap, KeyMap)
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Graphics.DSL (ViewPort (..))
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
import Renderer (renderPrimitive)
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
      rexp = compileModuleGraph nameMap valueFor grVisInfo (Nothing, mhover)
  R.save
  R.rectangle 0 0 modGraphWidth modGraphHeight
  R.clip
  let vpi = mgrui ^. modGraphViewPort
      ViewPort (vx0, vy0) (vx1, vy1) = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
      scaleX = modGraphWidth / (vx1 - vx0)
      scaleY = modGraphHeight / (vy1 - vy0)
  R.scale scaleX scaleY
  R.translate (-vx0) (-vy0)
  traverse_ (renderPrimitive uiRef vb) rexp
  R.restore
