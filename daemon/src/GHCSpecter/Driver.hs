module GHCSpecter.Driver
  ( startComm,
  )
where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    newTChanIO,
    newTQueueIO,
    newTVar,
  )
import GHCSpecter.Config (Config (..))
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session.Types (ServerSession (..))
import GHCSpecter.Driver.Worker qualified as Worker
import GHCSpecter.Server.Types (initServerState)

startComm :: Config -> IO ServerSession
startComm cfg = do
  let socketFile = configSocket cfg
      nodeSizeLimit = configModuleClusterSize cfg
  ssRef <- atomically $ newTVar (initServerState nodeSizeLimit)
  workQ <- newTQueueIO
  chanSignal <- newTChanIO
  let servSess = ServerSession ssRef chanSignal
  _ <- forkOS $ Comm.listener socketFile servSess workQ
  _ <- forkOS $ Worker.runWorkQueue workQ
  pure servSess
