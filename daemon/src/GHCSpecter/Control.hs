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
  ( Event (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    TimingEvent (..),
  )

handleEvent :: Event -> UTCTime -> Control ()
handleEvent topEv stepStartTime = do
  (oldUI, oldSS) <- getState
  (newUI, newSS) <-
    case oldUI ^. uiView of
      -- TODO: should not have this case.
      BannerMode -> pure (oldUI, oldSS)
      MainMode oldMainView -> do
        (newMainView, newSS, mLastUpdatedUI) <- handleMainEvent (oldMainView, oldSS)
        let newUI = (uiView .~ MainMode newMainView) oldUI
            newUI' =
              case mLastUpdatedUI of
                Nothing -> newUI
                Just t ->
                  if newUI ^. uiShouldUpdate
                    then (uiLastUpdated .~ t) newUI
                    else newUI
        pure (newUI', newSS)
  putState (newUI, newSS)
  where
    handleMainEvent (oldMainView, oldSS) =
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
        MessageChanUpdated -> do
          let newSS = (serverShouldUpdate .~ True) oldSS
          pure (oldMainView, newSS, Nothing)
        UITick -> do
          pure (oldMainView, oldSS, Nothing)

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

bannerMode :: UTCTime -> Control ()
bannerMode startTime = do
  (ui, ss) <- getState
  let ui' = (uiView .~ BannerMode) ui
  putState (ui', ss)
  go
  (ui1, ss1) <- getState
  let ui1' = (uiView .~ MainMode emptyMainView) ui1
  putState (ui1', ss1)
  where
    duration = Clock.secondsToNominalDiffTime 1.0
    go = do
      now <- getCurrentTime
      if now `Clock.diffUTCTime` startTime < duration
        then do
          ev <- nextEvent
          printMsg (T.pack (show ev))
          go
        else pure ()

main :: Control ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  bannerMode clientSessionStartTime

  bannerEndTime <- getCurrentTime
  printMsg $ "banner mode ends at " <> T.pack (show bannerEndTime)

  forever $ do
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
