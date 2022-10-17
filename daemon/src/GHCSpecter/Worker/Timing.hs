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
  )
import Control.Lens (to, (.~), (^.))
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Data.Timing.Util
  ( makeBlockerGraph,
    makeTimingTable,
  )
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
  atomically $ do
    ss <- readTVar ssRef
    let mgi = ss ^. serverSessionInfo . to sessionModuleGraph
        ttable = ss ^. serverTiming . tsTimingTable
        blockerGraph = makeBlockerGraph mgi ttable
    modifyTVar' ssRef (serverTiming . tsBlockerGraph .~ blockerGraph)
