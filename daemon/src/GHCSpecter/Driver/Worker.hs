module GHCSpecter.Driver.Worker
  ( runWorkQueue,
  )
where

import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTChanIO,
    newTQueueIO,
    newTVar,
    newTVarIO,
    readTChan,
    readTQueue,
    writeTChan,
  )

runWorkQueue :: TQueue (IO ()) -> IO ()
runWorkQueue workQ = go 0
  where
    go :: Int -> IO ()
    go n = do
      job <- atomically $ readTQueue workQ
      -- TODO: disabled as it's too verbose. enable with proper logging system.
      -- putStrLn $ show n ++ "th job started"
      job
      -- putStrLn $ show n ++ "th job ended"
      threadDelay 50_000
      go (n + 1)
