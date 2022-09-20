module GHCSpecter.Control
  ( main,
  )
where

import Control.Lens ((.~), (^.), _1, _2)
import Control.Monad.Extra (loopM)
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
    refreshUIAfter,
    saveSession,
    shouldUpdate,
    type Control,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )
import GHCSpecter.UI.Constants (uiUpdateInterval)
import GHCSpecter.UI.Types
  ( HasMainView (..),
    HasModuleGraphUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIState (..),
    MainView,
    ModuleGraphUI (..),
    UIView (..),
    emptyMainView,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    Event (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    TimingEvent (..),
  )

updateMainView ::
  Event ->
  UTCTime ->
  (MainView, ServerState) ->
  Control (MainView, ServerState, Maybe UTCTime)
updateMainView topEv stepStartTime (oldMainView, oldSS) =
  case topEv of
    TabEv tab' -> do
      let newMainView = (mainTab .~ tab') oldMainView
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView, newSS, Nothing)
    ExpandModuleEv mexpandedModu' -> do
      let newMainView = (mainSourceView . srcViewExpandedModule .~ mexpandedModu') oldMainView
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView, newSS, Nothing)
    MainModuleEv ev -> do
      let mgui = oldMainView ^. mainMainModuleGraph
      (mgui', mxy) <- handleModuleGraphEv ev mgui
      let newMainView = (mainMainModuleGraph .~ mgui') oldMainView
          (newMainView', mt) = handleMouseMove newMainView mxy
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView', newSS, mt)
    SubModuleEv sev ->
      case sev of
        SubModuleGraphEv ev -> do
          let mgui = oldMainView ^. mainSubModuleGraph . _2
          (mgui', mxy) <- handleModuleGraphEv ev mgui
          let newMainView = (mainSubModuleGraph . _2 .~ mgui') oldMainView
              (newMainView', mt) = handleMouseMove newMainView mxy
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newMainView', newSS, mt)
        SubModuleLevelEv d' -> do
          let newMainView = (mainSubModuleGraph . _1 .~ d') oldMainView
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newMainView, newSS, Nothing)
    SessionEv SaveSessionEv -> do
      saveSession
      let newSS = (serverShouldUpdate .~ False) oldSS
      pure (oldMainView, newSS, Nothing)
    SessionEv ResumeSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = False}
          newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
      pure (oldMainView, newSS, Nothing)
    SessionEv PauseSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = True}
          newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
      pure (oldMainView, newSS, Nothing)
    TimingEv (UpdateSticky b) -> do
      let newMainView = (mainTiming . timingUISticky .~ b) oldMainView
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView, newSS, Nothing)
    TimingEv (UpdatePartition b) -> do
      let newMainView = (mainTiming . timingUIPartition .~ b) oldMainView
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView, newSS, Nothing)
    TimingEv (UpdateParallel b) -> do
      let newMainView = (mainTiming . timingUIHowParallel .~ b) oldMainView
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newMainView, newSS, Nothing)
    BkgEv MessageChanUpdated -> do
      let newSS = (serverShouldUpdate .~ True) oldSS
      pure (oldMainView, newSS, Nothing)
    BkgEv RefreshUI -> do
      pure (oldMainView, oldSS, Nothing)
  where
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
      MainView ->
      Maybe (UTCTime, (Double, Double)) ->
      (MainView, Maybe UTCTime)
    handleMouseMove view mtxy =
      case mtxy of
        Nothing -> (view, Nothing)
        Just (t, xy) ->
          let view' = (mainMousePosition .~ xy) view
           in (view', Just t)

-- | showing ghc-specter banner in the beginning
showBanner :: Control ()
showBanner = do
  startTime <- getCurrentTime
  (ui, ss) <- getState
  let ui' = (uiView .~ BannerMode 0) ui
  putState (ui', ss)
  refreshUIAfter 0.1
  go startTime
  where
    duration = Clock.secondsToNominalDiffTime 2.0
    go start = do
      now <- getCurrentTime
      let diff = now `Clock.diffUTCTime` start
      if diff < duration
        then do
          _ev <- nextEvent
          (ui, ss) <- getState
          let r = realToFrac (diff / duration)
              ui' = (uiView .~ BannerMode r) ui
          putState (ui', ss)
          refreshUIAfter 0.1
          go start
        else pure ()

-- | main loop
mainLoop :: Control ()
mainLoop = do
  (ui1, ss1) <- getState
  let ui1' = (uiView .~ MainMode emptyMainView) ui1
  putState (ui1', ss1)

  flip loopM emptyMainView $ \oldMainView -> do
    lastUpdatedUI <- getLastUpdatedUI
    stepStartTime <- getCurrentTime

    -- wait for update interval, not to have too frequent update
    if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
      then shouldUpdate True
      else shouldUpdate False

    printMsg "waiting for the next event"
    ev <- nextEvent
    printMsg (T.pack (show ev))
    (oldUI, oldSS) <- getState

    (newMainView, newSS, mLastUpdatedUI) <-
      updateMainView ev stepStartTime (oldMainView, oldSS)
    let newUI = (uiView .~ MainMode newMainView) oldUI
        newUI' =
          case mLastUpdatedUI of
            Nothing -> newUI
            Just t ->
              if newUI ^. uiShouldUpdate
                then (uiLastUpdated .~ t) newUI
                else newUI
    putState (newUI', newSS)
    printMsg "commit new state"
    pure (Left newMainView)

main :: Control ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  -- show banner
  showBanner

  -- enter the main loop
  mainLoop
