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
import Control.Lens (to, (.~), (^.))
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Data.Timing.Util (makeTimingTable)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  case ss ^. serverSessionInfo . to sessionStartTime of
    Nothing -> pure ()
    Just sessStart -> do
      let timing = ss ^. serverTiming
          drvModMap = ss ^. serverDriverModuleMap
          ttable = makeTimingTable timing drvModMap sessStart
      atomically $ modifyTVar' ssRef (serverTimingTable .~ ttable)
