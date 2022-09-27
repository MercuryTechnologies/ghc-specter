module GHCSpecter.Driver.Worker
  ( runWorkQueue,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TQueue,
    atomically,
    readTQueue,
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
