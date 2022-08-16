{-# OPTIONS_GHC -fplugin Plugin.TypeCheck -fplugin Plugin.Timing #-}

module B (test2) where

import A (test)

test2 :: IO ()
test2 = test
