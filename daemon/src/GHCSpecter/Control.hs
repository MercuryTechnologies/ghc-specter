module GHCSpecter.Control
  ( main,
  )
where

import Control.Lens ((.~), (^.), _1, _2)
import Control.Monad (forever)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel (SessionInfo (..))
import GHCSpecter.Control.Types
  ( getCurrentTime,
    getLastUpdatedUI,
    getState,
    nextEvent,
    printMsg,
    putState,
    saveSession,
    shouldUpdate,
    type Control,
  )
import GHCSpecter.Server.Types (HasServerState (..))
import GHCSpecter.UI.Constants (uiUpdateInterval)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIState (..),
    ModuleGraphUI (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( Event (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    TimingEvent (..),
  )

handleEvent :: Event -> UTCTime -> Control ()
handleEvent topEv stepStartTime = do
  (oldUI, oldSS) <- getState
  (newUI, newSS) <- handleMainEvent (oldUI, oldSS)
  putState (newUI, newSS)
  where
    handleMainEvent (oldUI, oldSS) =
      case topEv of
        TabEv tab' -> do
          let newUI = (uiTab .~ tab') oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        ExpandModuleEv mexpandedModu' -> do
          let newUI = (uiSourceView . srcViewExpandedModule .~ mexpandedModu') oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        MainModuleEv ev -> do
          let mgui = oldUI ^. uiMainModuleGraph
          (mgui', mxy) <- handleModuleGraphEv ev mgui
          let newUI = (uiMainModuleGraph .~ mgui') oldUI
              newUI' = handleMouseMove newUI mxy
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI', newSS)
        SubModuleEv sev ->
          case sev of
            SubModuleGraphEv ev -> do
              let mgui = oldUI ^. uiSubModuleGraph . _2
              (mgui', mxy) <- handleModuleGraphEv ev mgui
              let newUI = (uiSubModuleGraph . _2 .~ mgui') oldUI
                  newUI' = handleMouseMove newUI mxy
                  newSS = (serverShouldUpdate .~ False) oldSS
              pure (newUI', newSS)
            SubModuleLevelEv d' -> do
              let newUI = (uiSubModuleGraph . _1 .~ d') oldUI
                  newSS = (serverShouldUpdate .~ False) oldSS
              pure (newUI, newSS)
        SessionEv SaveSessionEv -> do
          saveSession
          let newSS = (serverShouldUpdate .~ False) oldSS
          pure (oldUI, newSS)
        SessionEv ResumeSessionEv -> do
          let sinfo = oldSS ^. serverSessionInfo
              sinfo' = sinfo {sessionIsPaused = False}
              newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
          pure (oldUI, newSS)
        SessionEv PauseSessionEv -> do
          let sinfo = oldSS ^. serverSessionInfo
              sinfo' = sinfo {sessionIsPaused = True}
              newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
          pure (oldUI, newSS)
        TimingEv (UpdateSticky b) -> do
          let newUI = (uiTiming . timingUISticky .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        TimingEv (UpdatePartition b) -> do
          let newUI = (uiTiming . timingUIPartition .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        TimingEv (UpdateParallel b) -> do
          let newUI = (uiTiming . timingUIHowParallel .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        MessageChanUpdated -> do
          let newSS = (serverShouldUpdate .~ True) oldSS
          pure (oldUI, newSS)

    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      Control (ModuleGraphUI, Maybe (UTCTime, (Double, Double)))
    handleModuleGraphEv (HoverOnModuleEv mhovered) mgui =
      pure ((modGraphUIHover .~ mhovered) mgui, Nothing)
    handleModuleGraphEv (ClickOnModuleEv mclicked) mgui =
      pure ((modGraphUIClick .~ mclicked) mgui, Nothing)
    handleModuleGraphEv (DummyEv mxy) mgui = do
      t <- getCurrentTime
      printMsg (T.pack (show (stepStartTime, t, mxy)))
      pure (mgui, (t,) <$> mxy)

    handleMouseMove ::
      UIState ->
      Maybe (UTCTime, (Double, Double)) ->
      UIState
    handleMouseMove ui_ mtxy =
      case mtxy of
        Nothing -> ui_
        Just (t, xy) ->
          if ui_ ^. uiShouldUpdate
            then (uiLastUpdated .~ t) . (uiMousePosition .~ xy) $ ui_
            else uiMousePosition .~ xy $ ui_

main :: Control ()
main = forever $ do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime

  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

  printMsg "waiting for the next event"
  ev <- nextEvent
  printMsg (T.pack (show ev))
  handleEvent ev stepStartTime
