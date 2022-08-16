{-# OPTIONS_GHC -fplugin Plugin.TypeCheck -fplugin Plugin.Timing #-}
module C (test3) where

import A (test)
import B (test2)

test3 :: IO ()
test3 = do
  test
  test2
