{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Control.Concurrent
  ( forkIO,
    forkOS,
    threadDelay,
  )
import Control.Concurrent.STM
  ( atomically,
    newTChanIO,
    newTQueueIO,
    newTVarIO,
    readTVar,
  )
import Control.Monad (forever)
import Data.IORef (newIORef)
import Data.List qualified as L
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
import GHCSpecter.Graphics.DSL (Scene (..))
import GHCSpecter.UI.Types
  ( UIModel (..),
    UIState (..),
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event (Tab (..))
--
import ImGuiMain (uiMain)
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
  putStrLn "ghc-specter-daemon"

  withConfig Nothing $ \cfg -> do
    -- starting communication channel for plugin
    servSess <- startComm cfg

    initTime <- getCurrentTime
    let ui0 = emptyUIState initTime
        model = (ui0._uiModel) {_modelTab = TabModuleGraph}
        ui = ui0 {_uiModel = model}
    uiRef <- newTVarIO ui
    chanState <- newTChanIO
    chanQEv <- newTQueueIO

    let cliSess = ClientSession uiRef chanQEv

    -- prepare runner
    -- TODO: make common initialization function (but backend-dep)
    counterRef <- newIORef 0
    emref <- newTVarIO []

    let runHandler =
          RunnerHandler
            { runHandlerRefreshAction = pure (),
              runHandlerHitScene = \xy -> do
                putStrLn "in runHandlerHitScene"
                memap <-
                  atomically $ do
                    emaps <- readTVar emref
                    -- TODO: for now, just for testing
                    let memap = L.find (\emap -> emap.sceneId == "main-module-graph") emaps
                    pure memap
                case memap of
                  Nothing -> putStrLn "nothing?"
                  Just _ -> putStrLn "Just!"
                pure memap,
              runHandlerGetScene = \name ->
                atomically $ do
                  emaps <- readTVar emref
                  -- TODO: for now, just for testing
                  let memap = L.find (\emap -> emap.sceneId == name) emaps
                  pure memap,
              runHandlerAddToStage = \_ -> pure ()
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
    uiMain servSess cliSess emref
