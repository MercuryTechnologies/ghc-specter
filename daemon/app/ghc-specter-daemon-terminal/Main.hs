{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( newTQueueIO,
    newTVarIO,
  )
import Data.IORef (newIORef)
import Data.Time.Clock (getCurrentTime)
import GHCSpecter.Config (withConfig)
import GHCSpecter.Control qualified as Control
import GHCSpecter.Control.Runner
  ( RunnerEnv (..),
    RunnerHandler (..),
  )
import GHCSpecter.Driver (startComm)
import GHCSpecter.Driver.Session qualified as Session (main)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
  )
import GHCSpecter.Driver.Terminal qualified as Terminal
import GHCSpecter.UI.Types (emptyUIState)
import System.IO
  ( BufferMode (..),
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  hPutStrLn stderr "ghc-specter-daemon-terminal"
  withConfig Nothing $ \cfg -> do
    -- starting communication channel for plugin
    servSess <- startComm cfg

    initTime <- getCurrentTime
    let ui0 = emptyUIState initTime
    uiRef <- newTVarIO ui0
    chanQEv <- newTQueueIO

    let cliSess = ClientSession uiRef chanQEv

    -- prepare runner
    -- TODO: make common initialization function (but backend-dep)
    counterRef <- newIORef 0
    let runHandler =
          RunnerHandler
            { runHandlerRefreshAction = pure (),
              runHandlerHitScene = \_ -> pure Nothing,
              runHandlerGetScene = \_ -> pure Nothing,
              runHandlerAddToStage = \_ -> pure (),
              runHandlerScrollDownConsoleToEnd = pure ()
            }
        runner =
          RunnerEnv
            { runnerCounter = counterRef,
              runnerUIState = cliSess._csUIStateRef,
              runnerServerState = servSess._ssServerStateRef,
              runnerQEvent = cliSess._csPublisherEvent,
              runnerSignalChan = servSess._ssSubscriberSignal,
              runnerHandler = runHandler
            }

    -- start control-loop on a separate thread
    _ <-
      forkOS $
        Session.main runner servSess cliSess Control.mainLoop

    -- start UI loop
    Terminal.main cliSess
