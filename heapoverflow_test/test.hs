{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Control.Exception
  ( AsyncException (HeapOverflow),
    catch,
  )
import Data.List (foldl)
import GHC.Debug.Stub
  ( pause,
    withGhcDebug,
  )

main' :: IO ()
main' = do
  forkIO $ do
    let x = foldl (+) 0 [0..1_000_000_000]
    print x
  threadDelay 10_000_000

main :: IO ()
main =
  withGhcDebug $ 
    catch main' $ \(e :: AsyncException) ->
      case e of
        HeapOverflow -> do
          print "yay! HeapOverflow!"
          print "i am pausing"
          pause
        _ -> pure ()
