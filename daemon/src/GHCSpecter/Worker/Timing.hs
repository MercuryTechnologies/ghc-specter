module GHCSpecter.Worker.Timing
  ( timingWorker,
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    readTVar,
  )
import Control.Lens ((.~), (^.))
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Data.Timing.Util (makeTimingTable)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  let sessInfo = ss ^. serverSessionInfo
  case sessionStartTime sessInfo of
    Nothing -> pure ()
    Just sessStart -> do
      let timing = ss ^. serverTiming
          drvModMap = ss ^. serverDriverModuleMap
          mgi = sessionModuleGraph sessInfo
      ttable <- makeTimingTable timing drvModMap mgi sessStart
      atomically $ modifyTVar' ssRef (serverTimingTable .~ ttable)
