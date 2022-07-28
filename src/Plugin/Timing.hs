-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Env
  ( Hsc,
    HscEnv (..),
  )
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Pipeline (PhasePlus (HscOut), runPhase)
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
import GHC.Types.Target (targetId)
import GHC.Unit.Env
  ( UnitEnv (..),
  )
import GHC.Unit.Home
  ( GenHomeUnit (..),
  )
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Module.Status (HscStatus (..))
import GHC.Unit.Types (GenModule (moduleName))
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Toolbox.Channel
  ( ChanMessage (CMTiming),
    ChanMessageBox (..),
    type Session,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (printPpr, showPpr)

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
  let dflags = hsc_dflags env
  -- A and B
  traverse_ (printPpr dflags . targetId) (hsc_targets env)
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
  printPpr dflags (ghcMode dflags)
  print (outputFile dflags)
  printPpr dflags (homeUnitId_ dflags)
  printPpr dflags (mainModuleNameIs dflags)
  let hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        liftIO $ do
          putStrLn $ "###########" <> showPpr dflags phase
          case phase of
            HscOut _ _ status ->
              case status of
                HscNotGeneratingCode {} ->
                  print "HscNotGeneratingCode"
                HscUpToDate {} ->
                  print "HscUpToDate"
                HscUpdateBoot {} ->
                  print "HscUpdateBoot"
                HscUpdateSig {} ->
                  print "HscUpdateSig"
                HscRecomp {} ->
                  print "HscRecomp"
            _ -> pure ()
        runPhase phase fp
        -- pure (phase, fp)
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'

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
