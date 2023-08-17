module GHCSpecter.Driver.ConsoleOnly
  ( main,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHCSpecter.Driver.Session.Types (ClientSession)

-- | This console-only driver is only for developments and tests
main :: ClientSession -> IO ()
main _cliSess = do
  forever $
    threadDelay 100_000_000
