{-# OPTIONS_GHC -fplugin Plugin.CheckImports #-}

module B (testInB) where

import A

testInB :: IO ()
testInB = test
