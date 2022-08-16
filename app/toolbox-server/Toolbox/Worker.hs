module Toolbox.Worker
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Render.ModuleGraph (layOutGraph)
import Toolbox.Server.Types
  ( GraphVisInfo (..),
    ServerState (..),
    incrementSN,
  )
import Toolbox.Util.Graph (makeReducedGraphReversedFromModuleGraph)

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var graphInfo = do
  let modNameMap = mginfoModuleNameMap graphInfo
      reducedGraphReversed =
        makeReducedGraphReversedFromModuleGraph graphInfo
  grVisInfo <- layOutGraph modNameMap reducedGraphReversed
  atomically $
    modifyTVar' var (\ss -> incrementSN (ss {serverModuleGraph = Just grVisInfo}))
