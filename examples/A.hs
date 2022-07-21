{-# OPTIONS_GHC -fplugin Plugin.CheckImports #-}

module A (test) where

test :: IO ()
test = putStrLn "test"
