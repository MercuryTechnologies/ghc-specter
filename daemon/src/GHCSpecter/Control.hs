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
    HasUIModel (..),
    HasUIState (..),
    ModuleGraphUI (..),
    UIModel,
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

updateModel ::
  Event ->
  UTCTime ->
  (UIModel, ServerState) ->
  Control (UIModel, ServerState, Maybe UTCTime)
updateModel topEv stepStartTime (oldModel, oldSS) =
  case topEv of
    TabEv _tab' -> do
      let newSS = (serverShouldUpdate .~ False) oldSS
      pure (oldModel, newSS, Nothing)
    ExpandModuleEv mexpandedModu' -> do
      let newModel = (modelSourceView . srcViewExpandedModule .~ mexpandedModu') oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS, Nothing)
    MainModuleEv ev -> do
      let mgui = oldModel ^. modelMainModuleGraph
      (mgui', mxy) <- handleModuleGraphEv ev mgui
      let newModel = (modelMainModuleGraph .~ mgui') oldModel
          (newModel', mt) = handleMouseMove newModel mxy
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel', newSS, mt)
    SubModuleEv sev ->
      case sev of
        SubModuleGraphEv ev -> do
          let mgui = oldModel ^. modelSubModuleGraph . _2
          (mgui', mxy) <- handleModuleGraphEv ev mgui
          let newModel = (modelSubModuleGraph . _2 .~ mgui') oldModel
              (newModel', mt) = handleMouseMove newModel mxy
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newModel', newSS, mt)
        SubModuleLevelEv d' -> do
          let newModel = (modelSubModuleGraph . _1 .~ d') oldModel
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newModel, newSS, Nothing)
    SessionEv SaveSessionEv -> do
      saveSession
      let newSS = (serverShouldUpdate .~ False) oldSS
      pure (oldModel, newSS, Nothing)
    SessionEv ResumeSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = False}
          newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
      pure (oldModel, newSS, Nothing)
    SessionEv PauseSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = True}
          newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
      pure (oldModel, newSS, Nothing)
    TimingEv (UpdateSticky b) -> do
      let newModel = (modelTiming . timingUISticky .~ b) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS, Nothing)
    TimingEv (UpdatePartition b) -> do
      let newModel = (modelTiming . timingUIPartition .~ b) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS, Nothing)
    TimingEv (UpdateParallel b) -> do
      let newModel = (modelTiming . timingUIHowParallel .~ b) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS, Nothing)
    BkgEv MessageChanUpdated -> do
      let newSS = (serverShouldUpdate .~ True) oldSS
      pure (oldModel, newSS, Nothing)
    BkgEv RefreshUI -> do
      pure (oldModel, oldSS, Nothing)
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
      UIModel ->
      Maybe (UTCTime, (Double, Double)) ->
      (UIModel, Maybe UTCTime)
    handleMouseMove model mtxy =
      case mtxy of
        Nothing -> (model, Nothing)
        Just (t, xy) ->
          let model' = (modelMousePosition .~ xy) model
           in (model', Just t)

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
  let model1' = ui1' ^. uiModel
  flip loopM (emptyMainView, model1') $ \(oldMainView, oldModel) -> do
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
    let newMainView =
          case ev of
            TabEv tab' -> (mainTab .~ tab') oldMainView
            _ -> oldMainView
    (newModel, newSS, mLastUpdatedUI) <-
      updateModel ev stepStartTime (oldModel, oldSS)
    let newUI =
          (uiView .~ MainMode newMainView)
            . (uiModel .~ newModel)
            $ oldUI
        newUI' =
          case mLastUpdatedUI of
            Nothing -> newUI
            Just t ->
              if newUI ^. uiShouldUpdate
                then (uiLastUpdated .~ t) newUI
                else newUI
    putState (newUI', newSS)
    printMsg "commit new state"
    pure (Left (newMainView, newModel))

main :: Control ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  -- show banner
  showBanner

  -- enter the main loop
  mainLoop
