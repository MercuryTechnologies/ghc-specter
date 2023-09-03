{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

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
  )
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import GHC.Driver.Backend (backendDescription)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Session qualified as GHC
import GHC.RTS.Flags (getRTSFlags)
import GHC.Utils.CliOption (showOpt)
import GHCSpecter.Channel.Outbound.Types
  ( Backend (..),
    ChanMessage (..),
    DynFlagsInfo (..),
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
import Plugin.GHCSpecter.Comm (queueMessage, runMessageQueue)
import Plugin.GHCSpecter.Types
  ( PluginSession (..),
    initMsgQueue,
    sessionRef,
  )
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Process (getCurrentPid)

mkDynFlagsInfo :: HscEnv -> DynFlagsInfo
mkDynFlagsInfo env = DynFlagsInfo (T.pack str)
  where
    dflags = hsc_dflags env
    showWithOpts (s, os) = L.intercalate " " (s : fmap showOpt os)
    str =
      show (GHC.settings dflags)
        <> "\nprogramName = "
        <> GHC.programName dflags
        <> "\nprojectVersion = "
        <> GHC.projectVersion dflags
        <> "\nghcUsagePath = "
        <> GHC.ghcUsagePath dflags
        <> "\nghciUsagePath = "
        <> GHC.ghciUsagePath dflags
        <> "\ntopDir = "
        <> GHC.topDir dflags
        <> "\nextraGccViaCFlags = "
        <> show (GHC.extraGccViaCFlags dflags)
        <> "\nglobalPackageDatabasePath = "
        <> GHC.globalPackageDatabasePath dflags
        <> "\npgm_L = "
        <> GHC.pgm_L dflags
        <> "\npgm_P = "
        <> showWithOpts (GHC.pgm_P dflags)
        <> "\npgm_F = "
        <> GHC.pgm_F dflags
        <> "\npgm_c = "
        <> GHC.pgm_c dflags
        <> "\npgm_cxx = "
        <> GHC.pgm_cxx dflags
        <> "\npgm_a = "
        <> showWithOpts (GHC.pgm_a dflags)
        <> "\npgm_l = "
        <> showWithOpts (GHC.pgm_l dflags)
        <> "\npgm_lm = "
        <> maybe "" showWithOpts (GHC.pgm_lm dflags)
        <> "\npgm_dll = "
        <> showWithOpts (GHC.pgm_dll dflags)
        <> "\npgm_T = "
        <> GHC.pgm_T dflags
        <> "\npgm_windres = "
        <> GHC.pgm_windres dflags
        <> "\npgm_lcc = "
        <> showWithOpts (GHC.pgm_lcc dflags)
        <> "\npgm_ar = "
        <> GHC.pgm_ar dflags
        <> "\npgm_ranlib = "
        <> GHC.pgm_ranlib dflags
        <> "\npgm_lo = "
        <> showWithOpts (GHC.pgm_lo dflags)
        <> "\npgm_lc = "
        <> showWithOpts (GHC.pgm_lc dflags)
        <> "\npgm_i = "
        <> GHC.pgm_i dflags
        <> "\npgm_lc = "
        <> GHC.pgm_ranlib dflags

-- let dynFlags_msg = mkDynFlagsInfo env

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
  print startTime
  let dynFlags_msg = mkDynFlagsInfo env
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
                let desc = backendDescription (GHC.backend (hsc_dflags env))
                 in if
                        | desc == "native code generator" -> NCG
                        | desc == "LLVM" -> LLVM
                        | desc == "compiling via C" -> ViaC
                        --  | desc == "compiling to JavaScript" -> Javascript
                        | desc == "byte-code interpreter" -> Interpreter
                        | otherwise -> NoBackend
              newGhcSessionInfo =
                SessionInfo
                  { sessionDynFlags = Just dynFlags_msg,
                    sessionProcess = Just (ProcessInfo pid execPath cwd args rtsflags),
                    sessionGhcMode = ghcMode,
                    sessionBackend = backend,
                    sessionStartTime = Just startTime,
                    sessionIsPaused = configStartWithBreakpoint cfg
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
  -- start message queue
  when isNewStart $ do
    putStrLn "##########################"
    putStrLn "ghc-specter plugin started"
    putStrLn "##########################"
    void $ forkOS $ runMessageQueue cfg queue
    sinfo <-
      atomically $
        psSessionInfo <$> readTVar sessionRef
    queueMessage (CMSession sinfo)
