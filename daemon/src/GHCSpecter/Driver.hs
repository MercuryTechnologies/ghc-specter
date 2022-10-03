module GHCSpecter.Driver
  ( webServer,
  )
where

import Concur.Core (Widget, liftSTM, unsafeBlockingIO)
import Control.Applicative ((<|>))
import Control.Concurrent.STM
  ( TChan,
    atomically,
    newTChanIO,
    newTVarIO,
    readTChan,
    writeTChan,
  )
import Control.Lens ((.~), (^.))
import Control.Monad.Extra (loopM)
import Data.Time.Clock (getCurrentTime)
import GHCSpecter.Config (Config (..))
import GHCSpecter.Data.Assets qualified as Assets
import GHCSpecter.Driver.Session qualified as Session
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
    UIChannel (..),
  )
import GHCSpecter.Render (render)
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.ConcurReplica.Run (runDefaultWithStyle)
import GHCSpecter.UI.ConcurReplica.Types
  ( IHTML,
    blockDOMUpdate,
    unblockDOMUpdate,
  )
import GHCSpecter.UI.Types
  ( HasUIState (..),
    UIState,
    UIView (..),
    emptyMainView,
    emptyUIState,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (RefreshUI),
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
  print styleText
  runDefaultWithStyle port "ghc-specter" styleText $
    \_ -> do
      uiRef <-
        unsafeBlockingIO $ do
          initTime <- getCurrentTime
          let ui0 = emptyUIState assets initTime
              ui0' = (uiView .~ MainMode emptyMainView) ui0
          newTVarIO ui0'
      chanEv <- unsafeBlockingIO newTChanIO
      chanState <- unsafeBlockingIO newTChanIO
      chanBkg <- unsafeBlockingIO newTChanIO
      let newCS = ClientSession uiRef chanEv chanState chanBkg
          newUIChan = UIChannel chanEv chanState chanBkg
      unsafeBlockingIO $ Session.main servSess newCS
      loopM (step newUIChan) (BkgEv RefreshUI)
  where
    -- A single step of the outer loop (See Note [Control Loops]).
    step ::
      -- UI comm channel
      UIChannel ->
      -- last event
      Event ->
      Widget IHTML (Either Event ())
    step (UIChannel chanEv chanState chanBkg) ev = do
      (ui, ss) <-
        unsafeBlockingIO $ do
          atomically $ writeTChan chanEv ev
          (ui, ss) <- atomically $ readTChan chanState
          pure (ui, ss)
      stepRender (ui, ss) <|> (Left . BkgEv <$> waitForBkgEv chanBkg)

    stepRender :: (UIState, ServerState) -> Widget IHTML (Either Event ())
    stepRender (ui, ss) = do
      let renderUI =
            if ui ^. uiShouldUpdate
              then unblockDOMUpdate (render (ui, ss))
              else blockDOMUpdate (render (ui, ss))
      Left <$> renderUI

    waitForBkgEv ::
      -- channel for receiving bkg event
      TChan BackgroundEvent ->
      Widget IHTML BackgroundEvent
    waitForBkgEv chanBkg = liftSTM $ readTChan chanBkg
