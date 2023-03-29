module ModuleGraph (
  renderModuleGraph,
) where

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
import GHCSpecter.Render.Components.GraphView (compileModuleGraph)
import GI.Cairo.Render qualified as R
import Types (ViewBackend (..))
import Util (renderPrimitive)

renderModuleGraph ::
  ViewBackend ->
  IntMap ModuleName ->
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId Timer ->
  [(Text, [Text])] ->
  GraphVisInfo ->
  R.Render ()
renderModuleGraph vw nameMap drvModMap timing clustering grVisInfo = do
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
      rexp = compileModuleGraph nameMap valueFor grVisInfo (Nothing, Nothing)
  traverse_ (renderPrimitive vw) rexp
