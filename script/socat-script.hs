module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    shell,
    waitForProcess,
  )

main :: IO ()
main = do
  let ghc_specter_daemon =
        ( shell "cabal run ghc-specter-daemon:exe:ghc-specter-daemon-console-only"
        )
          { std_in = Inherit,
            std_out = Inherit,
            std_err = CreatePipe
          }
  (_, _, Just herr, ph) <- createProcess ghc_specter_daemon

  let socat =
        (shell "socat - UNIX-CONNECT:/tmp/ghc-specter-stderr.ipc")
          { std_in = UseHandle herr
          }

  _ <- createProcess socat

  ex <- waitForProcess ph
  print ex
