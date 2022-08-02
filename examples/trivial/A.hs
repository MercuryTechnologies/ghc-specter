-- NOTE: To use a GHC plugin, one can turn it on by the GHC CLI arguments:
-- -fplugin (plugin_module).
-- If one specifies the arguments in the ghc-options field in the cabal file,
-- the plugin will be operated over all the modules in the package.
-- The plugin module should be visible to ghc, so the package that defines
-- the module should be a dependency in the cabal package, or alternatively,
-- -plugin-package (package_name) can be specified.
--
-- To turn on a plugin only for a specific module, one can use the OPTIONS_GHC pragma
-- with the plugin arguments like this example.
--
-- In this case, the Plugin.Trivial module will be invoked as plugin as the module
-- defines a function with the reserved keyword "plugin".
{-# OPTIONS_GHC -fplugin Plugin.Trivial #-}

module A (test) where

test :: IO ()
test = putStrLn "hello there"
