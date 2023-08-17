module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHCSpecter.Config (withConfig)
import GHCSpecter.Driver (startComm)

main :: IO ()
main = do
  putStrLn "ghc-specter daemon console only"
  withConfig Nothing $ \cfg -> do
    servSess <- startComm cfg
    -- webServer cfg servSess

    forever $
      threadDelay 100_000_000
