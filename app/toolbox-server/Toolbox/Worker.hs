module Toolbox.Worker
  ( moduleGraphWorker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Toolbox.Channel (ModuleGraphInfo (..))
import Toolbox.Render.ModuleGraph
  ( layOutGraph,
    makeReducedGraphReversedFromModuleGraph,
  )
import Toolbox.Server.Types
  ( GraphVisInfo (..),
    ServerState (..),
    incrementSN,
  )

moduleGraphWorker :: TVar ServerState -> ModuleGraphInfo -> IO ()
moduleGraphWorker var graphInfo = do
  let modNameMap = mginfoModuleNameMap graphInfo
      reducedGraphReversed =
        makeReducedGraphReversedFromModuleGraph graphInfo
  grVisInfo <- layOutGraph modNameMap reducedGraphReversed
  putStrLn $ "number of boxes : " ++ show (length (gviNodes grVisInfo))
  print grVisInfo
  atomically $
    modifyTVar' var (\ss -> incrementSN (ss {serverModuleGraph = Just grVisInfo}))
