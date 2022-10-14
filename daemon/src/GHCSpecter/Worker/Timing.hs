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
import Control.Lens ((.~))
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )
import GHCSpecter.Util.Timing (makeTimingTable)

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  let ttable = makeTimingTable ss
  atomically $ modifyTVar' ssRef (serverTimingTable .~ ttable)
