{-# OPTIONS_GHC -Werror #-}

-- This module provides the current module under compilation.
module Plugin.Timing
  ( -- NOTE: The name "plugin" should be used as a GHC plugin.
    plugin,
    sessionRef,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Hooks (runPhaseHook)
import GHC.Driver.Phases (Phase (StopLn))
import GHC.Driver.Pipeline
  ( PhasePlus (RealPhase),
    PipeState (iface),
    getPipeState,
    runPhase,
  )
import GHC.Driver.Plugins
  ( Plugin (..),
    defaultPlugin,
    type CommandLineOption,
  )
import GHC.Unit.Module.ModIface (ModIface_ (mi_module))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Unit.Types (GenModule (moduleName))
import System.IO.Unsafe (unsafePerformIO)
import Toolbox.Channel
  ( ChanMessage (CMSession, CMTiming),
    ChanMessageBox (..),
    SessionInfo (..),
    Timer (..),
    resetTimer,
  )
import Toolbox.Comm (runClient, sendObject)
import Toolbox.Util (showPpr)

plugin :: Plugin
plugin =
  defaultPlugin {driverPlugin = driver}

-- shared across the session
sessionRef :: IORef SessionInfo
{-# NOINLINE sessionRef #-}
sessionRef = unsafePerformIO (newIORef (SessionInfo Nothing))

-- TODO: until PR#9 is merged.
socketFile :: FilePath
socketFile = "/tmp/ghc-build-analyzer.ipc"

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ env = do
  let dflags = hsc_dflags env
  startTime <- getCurrentTime
  SessionInfo msessionStart <- readIORef sessionRef
  case msessionStart of
    Nothing -> do
      let startedSession = SessionInfo (Just startTime)
      writeIORef sessionRef startedSession
      runClient socketFile $ \sock ->
        sendObject sock $ CMBox (CMSession startedSession)
    _ -> pure ()
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
                runClient socketFile $ \sock ->
                  sendObject sock $ CMBox (CMTiming modName timer)
          _ -> pure ()

        pure (phase', fp')
      hooks' = hooks {runPhaseHook = Just runPhaseHook'}
      env' = env {hsc_hooks = hooks'}
  pure env'
