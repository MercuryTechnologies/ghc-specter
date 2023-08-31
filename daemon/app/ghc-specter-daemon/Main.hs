{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    newTQueueIO,
    newTVarIO,
    readTVar,
    writeTVar,
  )
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
import GHCSpecter.Graphics.DSL
  ( Scene (..),
    Stage (..),
  )
import GHCSpecter.UI.Constants (appWidgetConfig)
import GHCSpecter.UI.Types
  ( UIModel (..),
    UIState (..),
    emptyUIState,
  )
import GHCSpecter.Util.Transformation qualified as Transformation (hitScene)
import Render qualified (main)
import System.IO
  ( BufferMode (..),
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
        model = (ui0._uiModel) {_modelWidgetConfig = appWidgetConfig}
        ui = ui0 {_uiModel = model}
    uiRef <- newTVarIO ui
    chanQEv <- newTQueueIO

    let cliSess = ClientSession uiRef chanQEv

    -- prepare runner
    -- TODO: make common initialization function (but backend-dep)
    counter_ref <- newIORef 0
    em_ref <- newTVarIO []
    stage_ref <- newTVarIO (Stage [])
    console_scroll_ref <- newTVarIO False
    let runHandler =
          RunnerHandler
            { runHandlerRefreshAction = pure (),
              runHandlerHitScene = \xy ->
                atomically $ do
                  emaps <- readTVar em_ref
                  let memap = Transformation.hitScene xy emaps
                  pure memap,
              runHandlerGetScene = \name ->
                atomically $ do
                  emaps <- readTVar em_ref
                  let memap = L.find (\emap -> emap.sceneId == name) emaps
                  pure memap,
              runHandlerAddToStage = \scene -> atomically $ do
                Stage cfgs <- readTVar stage_ref
                let cfgs' = filter (\scene' -> scene.sceneId /= scene'.sceneId) cfgs
                    cfgs'' = scene : cfgs'
                writeTVar stage_ref (Stage cfgs''),
              runHandlerScrollDownConsoleToEnd =
                -- putStrLn "scrollDownConsoleToEnd called!"
                atomically $ writeTVar console_scroll_ref True
            }
        runner =
          RunnerEnv
            { runnerCounter = counter_ref,
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
    Render.main servSess cliSess (em_ref, stage_ref, console_scroll_ref)
