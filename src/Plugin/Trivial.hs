-- This module provides the current module under compilation.
module Plugin.Trivial
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import GHC.Driver.Env
  ( HscEnv (..),
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Driver.Session
  ( DynFlags
      ( ghcMode,
        homeUnitId_,
        mainModuleNameIs
      ),
    outputFile,
  )
import GHC.Unit.Env
  ( UnitEnv (..),
  )
import GHC.Unit.Home
  ( GenHomeUnit (..),
  )
import Toolbox.Util (printPpr)

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = driver
    }

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ env = do
  putStrLn "driver plugin is called"
  let uenv = hsc_unit_env env
      hu = ue_home_unit uenv
  case hu of
    DefiniteHomeUnit _ (Just (_, inst)) -> do
      putStrLn "definite: instantiated"
    DefiniteHomeUnit _ Nothing -> do
      putStrLn "definite: but no instantiation"
    IndefiniteHomeUnit {} -> putStrLn "indefinite"
  let dflags = hsc_dflags env
  printPpr dflags (ghcMode dflags)
  print (outputFile dflags)
  printPpr dflags (homeUnitId_ dflags)
  printPpr dflags (mainModuleNameIs dflags)
  pure env
