module Plugin.GHCSpecter.Init
  ( -- * init ghc session and send the info
    initGhcSession,
  )
where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    modifyTVar',
    readTVar,
    stateTVar,
  )
import Control.Monad (void, when)
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Backend qualified as GHC (Backend (..))
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session qualified as GHC (DynFlags (..), GhcMode (..))
import GHC.RTS.Flags (getRTSFlags)
import GHCSpecter.Channel.Outbound.Types
  ( Backend (..),
    ChanMessage (..),
    GhcMode (..),
    ProcessInfo (..),
    SessionInfo (..),
  )
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    emptyConfig,
    loadConfig,
  )
import GHCSpecter.Util.GHC
  ( extractModuleGraphInfo,
    extractModuleSources,
  )
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Types
  ( PluginSession (..),
    initMsgQueue,
    sessionRef,
  )
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Process (getCurrentPid)

-- TODO: Make the initialization work with GHCi.

-- | GHC session-wide initialization
initGhcSession :: HscEnv -> IO ()
initGhcSession env = do
  -- NOTE: Read session and overwrite on the session should be done in a single
  -- atomic STM operation. As a consequence, unfortunately, the necessary IO
  -- action should be done outside STM beforehand, which implies the action will
  -- be done multiple times per every driver start.
  -- This is because we do not have a plugin insertion at real GHC initialization.
  startTime <- getCurrentTime
  pid <- fromInteger . toInteger <$> getCurrentPid
  execPath <- getExecutablePath
  cwd <- canonicalizePath =<< getCurrentDirectory
  args <- getArgs
  rtsflags <- getRTSFlags
  queue_ <- initMsgQueue
  ecfg <- loadConfig defaultGhcSpecterConfigFile
  let cfg = either (const emptyConfig) id ecfg
  -- read/write should be atomic inside a single STM. i.e. no interleaving
  -- IO actions are allowed.
  (isNewStart, queue) <-
    atomically $ do
      ps <- readTVar sessionRef
      let ghcSessionInfo = psSessionInfo ps
          mtimeQueue =
            (,) <$> sessionStartTime ghcSessionInfo <*> psMessageQueue ps
      case mtimeQueue of
        -- session has started already.
        Just (_, queue) -> pure (False, queue)
        -- session start
        Nothing -> do
          let ghcMode =
                case (GHC.ghcMode (hsc_dflags env)) of
                  GHC.CompManager -> CompManager
                  GHC.OneShot -> OneShot
                  GHC.MkDepend -> MkDepend
              backend =
                case (GHC.backend (hsc_dflags env)) of
                  GHC.NCG -> NCG
                  GHC.LLVM -> LLVM
                  GHC.ViaC -> ViaC
                  GHC.Interpreter -> Interpreter
                  GHC.NoBackend -> NoBackend
              newGhcSessionInfo =
                SessionInfo
                  { sessionProcess = Just (ProcessInfo pid execPath cwd args rtsflags),
                    sessionGhcMode = ghcMode,
                    sessionBackend = backend,
                    sessionStartTime = Just startTime,
                    sessionIsPaused = configStartWithBreakpoint cfg,
                    sessionPreferredModuleClusterSize = Just (configModuleClusterSize cfg)
                  }
          modifyTVar'
            sessionRef
            ( \s ->
                s
                  { psSessionConfig = cfg,
                    psSessionInfo = newGhcSessionInfo,
                    psMessageQueue = Just queue_
                  }
            )
          pure (True, queue_)
  when isNewStart $ do
    void $ forkOS $ runMessageQueue cfg queue
    sinfo <-
      atomically $ psSessionInfo <$> readTVar sessionRef
    queueMessage (CMSession sinfo)
