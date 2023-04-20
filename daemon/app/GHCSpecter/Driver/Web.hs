{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Driver.Web (
  webServer,
) where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (
  TQueue,
  atomically,
  newTChanIO,
  newTQueueIO,
  newTVarIO,
  readTChan,
  readTQueue,
  writeTChan,
 )
import Control.Lens ((.~), (^.))
import Control.Monad.Extra (loopM)
import Data.IORef (newIORef)
import Data.Time.Clock (getCurrentTime)
import GHCSpecter.Config (Config (..))
import GHCSpecter.Control qualified as Control (main)
import GHCSpecter.Control.Runner (RunnerEnv (..))
import GHCSpecter.Data.Assets qualified as Assets
import GHCSpecter.Driver.Session qualified as Session
import GHCSpecter.Driver.Session.Types (
  ClientSession (..),
  HasClientSession (..),
  HasServerSession (..),
  ServerSession (..),
  UIChannel (..),
 )
import GHCSpecter.Render.Web (render)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.ConcurReplica.Run (runDefaultWithStyle)
import GHCSpecter.UI.ConcurReplica.Types (
  IHTML,
  blockDOMUpdate,
  unblockDOMUpdate,
 )
import GHCSpecter.UI.Types (
  HasUIModel (..),
  HasUIState (..),
  UIState,
  emptyUIState,
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (RefreshUI),
  Event (BkgEv),
 )

-- NOTE:
-- server state: shared across the session
-- ui state: per web view.
-- control: per web view

webServer :: Config -> ServerSession -> IO ()
webServer cfg servSess = do
  let port = configWebPort cfg
  assets <- Assets.loadAssets
  let styleText = assets ^. Assets.assetsGhcSpecterCss
  runDefaultWithStyle port "ghc-specter" styleText $
    \_ -> do
      uiRef <-
        unsafeBlockingIO $ do
          initTime <- getCurrentTime
          let ui0 = emptyUIState assets initTime
              ui0' = (uiModel . modelTransientBanner .~ Nothing) ui0
          newTVarIO ui0'
      chanEv <- unsafeBlockingIO newTChanIO
      chanState <- unsafeBlockingIO newTChanIO
      chanQEv <- unsafeBlockingIO newTQueueIO

      let cliSess = ClientSession uiRef chanEv chanState chanQEv
          newUIChan = UIChannel chanEv chanState chanQEv
      -- prepare runner
      -- TODO: make common initialization function (but backend-dep)
      counterRef <- unsafeBlockingIO $ newIORef 0
      let runner =
            RunnerEnv
              { runnerCounter = counterRef
              , runnerUIState = cliSess ^. csUIStateRef
              , runnerServerState = servSess ^. ssServerStateRef
              , runnerQEvent = cliSess ^. csPublisherEvent
              , runnerSignalChan = servSess ^. ssSubscriberSignal
              , runnerRefreshAction = pure ()
              , runnerHitScene = (\_ -> pure Nothing)
              }
      unsafeBlockingIO $ Session.main runner servSess cliSess Control.main
      loopM (step newUIChan) (BkgEv RefreshUI)
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      -- UI comm channel
      UIChannel ->
      -- last event
      Event ->
      Widget IHTML (Either Event ())
    step (UIChannel chanEv chanState chanQEv) ev = do
      (ui, ss) <-
        unsafeBlockingIO $ do
          atomically $ writeTChan chanEv ev
          (ui, ss) <- atomically $ readTChan chanState
          pure (ui, ss)
      stepRender (ui, ss) <|> (Left <$> waitForBkgEv chanQEv)

    stepRender :: (UIState, ServerState) -> Widget IHTML (Either Event ())
    stepRender (ui, ss) = do
      let renderUI =
            if ui ^. uiShouldUpdate
              then unblockDOMUpdate (render (ui, ss))
              else blockDOMUpdate (render (ui, ss))
      Left <$> renderUI

    waitForBkgEv ::
      -- queue for receiving event in other channel
      TQueue Event ->
      Widget IHTML Event
    waitForBkgEv chanQEv = liftSTM $ readTQueue chanQEv
