module Main where

import GHC.Debug.Client
  ( Debuggee,
    buildHeapGraph,
    gcRoots,
    precacheBlocks,
    withDebuggeeConnect,
    pause,
    ppHeapGraph,
    run,
  )

main :: IO ()
main = withDebuggeeConnect "/tmp/ghc-debug" p1

p1 :: Debuggee -> IO ()
p1 e = do
  pause e
  g <- run e $ do
        precacheBlocks
        (r:_) <- gcRoots
        buildHeapGraph (Just 10) r
  putStrLn (ppHeapGraph (const "") g)

