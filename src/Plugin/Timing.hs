-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Env
  ( Hsc,
    HscEnv (..),
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
import GHC.Hs (HsParsedModule)
import GHC.Unit.Env
  ( UnitEnv (..),
  )
import GHC.Unit.Home
  ( GenHomeUnit (..),
  )
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Toolbox.Channel
  ( ChanMessage (CMTiming),
    ChanMessageBox (..),
    type Session,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (printPpr)

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = driver
    , parsedResultAction = afterParser
    }

-- shared across the session
sessionRef :: IORef Session
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newIORef "not-initialized")

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ env = do
  session <- readIORef sessionRef
  print session
  writeIORef sessionRef "now-initialized"
  let uenv = hsc_unit_env env
      hu = ue_home_unit uenv
  case hu of
    DefiniteHomeUnit _ (Just (_, _inst)) -> do
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

afterParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
afterParser opts modSummary parsed = do
  let modName = T.pack $ moduleNameString $ moduleName $ ms_mod modSummary
  case opts of
    ipcfile : _ -> liftIO $ do
      session <- readIORef sessionRef      
      time <- getCurrentTime
      socketExists <- doesFileExist ipcfile
      when socketExists $
        runClient ipcfile $ \sock ->
          sendObject sock $ CMBox (CMTiming modName (session, time))          
    _ -> pure ()
  pure parsed
