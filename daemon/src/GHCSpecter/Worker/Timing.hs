module GHCSpecter.Worker.Timing
  ( timingWorker,
    timingBlockerGraphWorker,
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    readTVar,
    writeTVar,
  )
import Control.Lens (to, (.~), (^.))
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
  )
import GHCSpecter.Data.Timing.Util
  ( makeBlockerGraph,
    makeTimingTable,
  )
import GHCSpecter.GraphLayout.Algorithm.Builder (makeRevDep)
import GHCSpecter.GraphLayout.Sugiyama qualified as Sugiyama
import GHCSpecter.Server.Types
  ( HasServerState (..),
    HasTimingState (..),
    ServerState,
  )

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  let sessInfo = ss ^. serverSessionInfo
  case sessionStartTime sessInfo of
    Nothing -> pure ()
    Just sessStart -> do
      let timing = ss ^. serverTiming . tsTimingMap
          drvModMap = ss ^. serverDriverModuleMap
          mgi = sessionModuleGraph sessInfo
          ttable = makeTimingTable timing drvModMap mgi sessStart
      atomically $ modifyTVar' ssRef (serverTiming . tsTimingTable .~ ttable)

timingBlockerGraphWorker :: TVar ServerState -> IO ()
timingBlockerGraphWorker ssRef = do
  ss' <-
    atomically $ do
      ss <- readTVar ssRef
      let mgi = ss ^. serverSessionInfo . to sessionModuleGraph
          ttable = ss ^. serverTiming . tsTimingTable
          blockerGraph = makeBlockerGraph mgi ttable
          ss' = (serverTiming . tsBlockerGraph .~ blockerGraph) ss
      writeTVar ssRef ss'
      pure ss'
  let mgi = ss' ^. serverSessionInfo . to sessionModuleGraph
      modNameMap = mginfoModuleNameMap mgi
      blockerReversed = makeRevDep (ss' ^. serverTiming . tsBlockerGraph)
  grVisInfo <- Sugiyama.layOutGraph modNameMap blockerReversed
  atomically $
    modifyTVar' ssRef (serverTiming . tsBlockerGraphViz .~ Just grVisInfo)
