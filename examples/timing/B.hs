{-# OPTIONS_GHC -fplugin Plugin.CheckImports -fplugin Plugin.Timing #-}

module B (test2) where

import A (test)

test2 :: IO ()
test2 = test
