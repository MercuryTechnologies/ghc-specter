-- NOTE: To use a GHC plugin, one can turn it on by the GHC CLI arguments:
-- -fplugin (plugin_module).
-- If one specifies the arguments in the ghc-options field in the cabal file,
-- the plugin will be operated over all the modules in the package.
--
-- To turn on a plugin only for a specific module, one can use the OPTIONS_GHC pragma
-- with the plugin arguments like this example.
--
-- In this case, the Plugin.CheckImports module will be invoked as plugin as the module
-- defines a function with the reserved keyword "plugin".
{-# OPTIONS_GHC -fplugin Plugin.CheckImports #-}

module A (test) where

test :: IO ()
test = putStrLn "test"
