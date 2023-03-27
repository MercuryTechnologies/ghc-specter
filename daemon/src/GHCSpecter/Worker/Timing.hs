module GHCSpecter.Worker.Timing (
  timingWorker,
  timingBlockerGraphWorker,
) where

import Control.Concurrent.STM (
  TVar,
  atomically,
  modifyTVar',
  readTVar,
  writeTVar,
 )
import Control.Lens (to, (.~), (^.))
import GHCSpecter.Channel.Outbound.Types (
  ModuleGraphInfo (..),
  SessionInfo (..),
 )
import GHCSpecter.Data.Timing.Util (
  makeBlockerGraph,
  makeTimingTable,
 )
import GHCSpecter.GraphLayout.Algorithm.Builder (makeRevDep)
import GHCSpecter.GraphLayout.Sugiyama qualified as Sugiyama
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState,
 )
import GHCSpecter.UI.Types.Event (blockerThreshold)

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  let sessInfo = ss ^. serverSessionInfo
  case sessionStartTime sessInfo of
    Nothing -> pure ()
    Just sessStart -> do
      let timing = ss ^. serverTiming . tsTimingMap
          drvModMap = ss ^. serverDriverModuleMap
          mgi = ss ^. serverModuleGraphState . mgsModuleGraphInfo
          ttable = makeTimingTable timing drvModMap mgi sessStart
      atomically $ modifyTVar' ssRef (serverTiming . tsTimingTable .~ ttable)

timingBlockerGraphWorker :: TVar ServerState -> IO ()
timingBlockerGraphWorker ssRef = do
  (ss', mgi) <-
    atomically $ do
      ss <- readTVar ssRef
      let blThre = ss ^. serverTiming . tsBlockerDetailLevel . to blockerThreshold
          mgi = ss ^. serverModuleGraphState . mgsModuleGraphInfo
          ttable = ss ^. serverTiming . tsTimingTable
          blockerGraph = makeBlockerGraph blThre mgi ttable
          ss' = (serverTiming . tsBlockerGraph .~ blockerGraph) ss
      writeTVar ssRef ss'
      pure (ss', mgi)
  let modNameMap = mginfoModuleNameMap mgi
      blockerReversed = makeRevDep (ss' ^. serverTiming . tsBlockerGraph)
  grVisInfo <- Sugiyama.layOutGraph modNameMap blockerReversed
  atomically $
    modifyTVar' ssRef (serverTiming . tsBlockerGraphViz .~ Just grVisInfo)
