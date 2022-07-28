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
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Driver.Env
  ( Hsc,
    HscEnv (..),
  )
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Phases (Phase (StopLn))
import GHC.Driver.Pipeline
  ( PhasePlus (HscOut, RealPhase),
    PipeState (iface),
    getPipeState,
    runPhase,
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
import GHC.Types.Target (targetId)
import GHC.Unit.Env
  ( UnitEnv (..),
  )
import GHC.Unit.Home
  ( GenHomeUnit (..),
  )
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Module.Status (HscStatus (..))
import GHC.Unit.Types (GenModule (moduleName))
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Toolbox.Channel
  ( ChanMessage (CMTiming),
    ChanMessageBox (..),
    Timer (..),
    resetTimer,
    type Session,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (printPpr, showPpr)

plugin :: Plugin
plugin =
  defaultPlugin {driverPlugin = driver}

-- shared across the session
sessionRef :: IORef Session
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newIORef "not-initialized")

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver opts env = do
  let dflags = hsc_dflags env
  let uenv = hsc_unit_env env
      hu = ue_home_unit uenv
  startTime <- getCurrentTime
  let timer0 = resetTimer {timerStart = Just startTime}
      hooks = hsc_hooks env
      runPhaseHook' phase fp = do
        liftIO $ do
          putStrLn $ "###########" <> showPpr dflags phase
        (phase', fp') <- runPhase phase fp
        liftIO $ putStrLn $ "------------>" <> showPpr dflags phase'

        case phase' of
          RealPhase StopLn -> do
            mmodName <-
              fmap (T.pack . moduleNameString . moduleName . mi_module) . iface
                <$> getPipeState
            case mmodName of
              Nothing -> pure ()
              Just modName -> liftIO $ do
                endTime <- getCurrentTime
                let timer = timer0 {timerEnd = Just endTime}
                case opts of
                  ipcfile : _ -> liftIO $ do
                    socketExists <- doesFileExist ipcfile
                    when socketExists $
                      runClient ipcfile $ \sock ->
                        sendObject sock $ CMBox (CMTiming modName timer)
                  _ -> pure ()
          _ -> pure ()

        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'
