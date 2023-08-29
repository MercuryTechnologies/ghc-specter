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
import GHCSpecter.UI.Types.Event (Tab (..))
import GHCSpecter.Util.Transformation qualified as Transformation (hitScene)
import Render qualified (main)
import System.IO
  ( BufferMode (..),
    hSetBuffering,
    stderr,
    stdout,
  )

{-
initializeUIState :: UTCTime -> UIState
initializeUIState initTime = ui0'
  where
    defVP = ViewPort (0, 0) (modGraphWidth, 0.5 * modGraphHeight)
    toOrigin = maybe defVP translateToOrigin
    vpSessionMain =
      toOrigin $ M.lookup "session-main" appWidgetConfig._wcfgSession
    vpSessionProcess =
      toOrigin $ M.lookup "session-process" appWidgetConfig._wcfgSession
    vpSessionRts =
      toOrigin $ M.lookup "session-rts" appWidgetConfig._wcfgSession
    vpMainModGraph =
      toOrigin $ M.lookup "main-module-graph" appWidgetConfig._wcfgModuleGraph
    vpSubModGraph =
      toOrigin $ M.lookup "sub-module-graph" appWidgetConfig._wcfgModuleGraph
    vpSrcModTree =
      toOrigin $ M.lookup "module-tree" appWidgetConfig._wcfgSourceView
    vpSrcSource =
      toOrigin $ M.lookup "source-view" appWidgetConfig._wcfgSourceView
    vpSrcSupp =
      toOrigin $ M.lookup "supple-view-contents" appWidgetConfig._wcfgSourceView
    vpConsole =
      toOrigin $ M.lookup  "console-main" appWidgetConfig._wcfgTopLevel

    ui0 = emptyUIState initTime
    model0 = ui0._uiModel
    sessionUI0 = ui0._uiModel._modelSession
    sessionUI0' =
      sessionUI0 { _sessionUIMainViewPort = ViewPortInfo vpSessionMain Nothing }
    timingUI0 = ui0._uiModel._modelTiming
    timingUI0' =
      timingUI0 { _

    model0' =
      model0
        { _modelTab = TabSession,
          _modelSession = sessionUI0',
          -- ...
          _modelTiming = timingUI0'
        }
    ui0' = ui0 { _uiModel = model0' }
-}

{-        & (uiModel . modelTab .~ TabModuleGraph)
          . ( uiModel . modelSession . sessionUIMainViewPort
                .~ ViewPortInfo vpSessionMain Nothing
            )
          . ( uiModel . modelSession . sessionUIProcessViewPort
                .~ ViewPortInfo vpSessionProcess Nothing
            )
          . ( uiModel . modelSession . sessionUIRtsViewPort
                .~ ViewPortInfo vpSessionRts Nothing
            )
          . ( uiModel . modelMainModuleGraph . modGraphViewPort
                .~ ViewPortInfo vpMainModGraph Nothing
            )
          . ( uiModel . modelSubModuleGraph . _2 . modGraphViewPort
                .~ ViewPortInfo vpSubModGraph Nothing
            )
          . (uiModel . modelWidgetConfig .~ appWidgetConfig)
          . ( uiModel . modelSourceView . srcViewModuleTreeViewPort
                .~ ViewPortInfo vpSrcModTree Nothing
            )
          . ( uiModel . modelSourceView . srcViewSourceViewPort
                .~ ViewPortInfo vpSrcSource Nothing
            )
          . ( uiModel . modelSourceView . srcViewSuppViewPort
                .~ ViewPortInfo vpSrcSupp Nothing
            )
          . (uiModel . modelTiming . timingUIPartition .~ True)
          . (uiModel . modelTiming . timingUIHowParallel .~ False)
          . (uiModel . modelConsole . consoleViewPort .~ ViewPortInfo vpConsole Nothing)
-}

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
                writeTVar stage_ref (Stage cfgs'')
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
    Render.main servSess cliSess (em_ref, stage_ref)
