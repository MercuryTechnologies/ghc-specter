module GHCSpecter.Control
  ( main,
  )
where

import Control.Lens ((%~), (&), (.~), (^.), _1, _2)
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
    MainView,
    ModuleGraphUI (..),
    UIModel,
    UIView (..),
    emptyMainView,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    Event (..),
    ModuleGraphEvent (..),
    MouseEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    Tab (..),
    TimingEvent (..),
  )

updateModel ::
  Event ->
  (UIModel, ServerState) ->
  Control (UIModel, ServerState, Maybe UTCTime)
updateModel topEv (oldModel, oldSS) =
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
          mgui' = handleModuleGraphEv ev mgui
          newModel = (modelMainModuleGraph .~ mgui') oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS, Nothing)
    SubModuleEv sev ->
      case sev of
        SubModuleGraphEv ev -> do
          let mgui = oldModel ^. modelSubModuleGraph . _2
              mgui' = handleModuleGraphEv ev mgui
              newModel = (modelSubModuleGraph . _2 .~ mgui') oldModel
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newModel, newSS, Nothing)
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
    _ -> pure (oldModel, oldSS, Nothing)
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      ModuleGraphUI
    handleModuleGraphEv (HoverOnModuleEv mhovered) = (modGraphUIHover .~ mhovered)
    handleModuleGraphEv (ClickOnModuleEv mclicked) = (modGraphUIClick .~ mclicked)

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

checkIfUpdatable :: Control ()
checkIfUpdatable = do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime
  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

-- NOTE: This function should not exist forever.
goCommon :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goCommon ev (view, model) = do
  (ui, ss) <- getState
  (model', ss', mLastUpdatedUI) <- updateModel ev (model, ss)
  let -- just placeholder
      view' = view
      ui1 =
        (uiView .~ MainMode view')
          . (uiModel .~ model')
          $ ui
      ui' =
        case mLastUpdatedUI of
          Nothing -> ui1
          Just t ->
            if ui1 ^. uiShouldUpdate
              then (uiLastUpdated .~ t) ui1
              else ui1
  putState (ui', ss')
  pure (view', model')

goSession :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goSession = goCommon

goModuleGraph :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goModuleGraph = goCommon

goSourceView :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goSourceView = goCommon

goTiming :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goTiming ev (view, model0) = do
  model <-
    case ev of
      MouseEv (MouseDown (Just (x, y))) -> do
        let (tx, ty) = model0 ^. modelTiming . timingUIXY
        onDragging (x, y) (tx, ty)
      _ -> pure model0
  (ui, ss) <- getState
  (model', ss', mLastUpdatedUI) <- updateModel ev (model, ss)
  let -- just placeholder
      view' = view
      ui1 =
        (uiView .~ MainMode view')
          . (uiModel .~ model')
          $ ui
      ui' =
        case mLastUpdatedUI of
          Nothing -> ui1
          Just t ->
            if ui1 ^. uiShouldUpdate
              then (uiLastUpdated .~ t) ui1
              else ui1
  putState (ui', ss')
  pure (view', model')
  where
    -- (x, y): mouse down point, (tx, ty): viewport origin
    onDragging (x, y) (tx, ty) = do
      checkIfUpdatable
      ev' <- nextEvent
      case ev' of
        MouseEv (MouseUp (Just (x', y'))) -> do
          let (deltaX, deltaY) = (x' - x, y' - y)
              model =
                model0
                  & modelTiming . timingUIXY .~ (tx - deltaX, ty - deltaY)
          pure model
        MouseEv (MouseMove (Just (x', y'))) -> do
          let (deltaX, deltaY) = (x' - x, y' - y)
              model =
                model0
                  & modelTiming . timingUIXY .~ (tx - deltaX, ty - deltaY)
          (ui0, ss) <- getState
          now <- getCurrentTime
          let ui1 = ui0 & (uiModel .~ model)
              ui =
                if ui1 ^. uiShouldUpdate
                  then ui1 & uiLastUpdated .~ now
                  else ui1
          putState (ui, ss)
          onDragging (x, y) (tx, ty)
        _ -> onDragging (x, y) (tx, ty)

-- | top-level branching through tab
branchTab :: Tab -> (MainView, UIModel) -> Control ()
branchTab tab (view, model) =
  case tab of
    TabSession -> branchLoop goSession
    TabModuleGraph -> branchLoop goModuleGraph
    TabSourceView -> branchLoop goSourceView
    TabTiming -> branchLoop goTiming
  where
    branchLoop go = do
      let view1 = (mainTab .~ tab) view
      (view', model') <- go (TabEv tab) (view1, model)
      loop (view', model')
      where
        loop (v, m) = do
          checkIfUpdatable
          printMsg "wait for the next event"
          ev <- nextEvent
          printMsg $ "event received: " <> T.pack (show ev)
          case ev of
            TabEv tab' -> branchTab tab' (v, m)
            _ -> do
              (v', m') <- go ev (v, m)
              loop (v', m')

initializeMainView :: Control (MainView, UIModel)
initializeMainView = do
  (ui1, ss1) <- getState
  let ui1' = (uiView .~ MainMode emptyMainView) ui1
  putState (ui1', ss1)
  pure (emptyMainView, ui1' ^. uiModel)

-- | main loop
mainLoop :: (MainView, UIModel) -> Control ()
mainLoop (view, model) = do
  checkIfUpdatable
  ev <- nextEvent
  printMsg (T.pack (show ev))
  case ev of
    TabEv tab -> branchTab tab (view, model)
    _ -> mainLoop (view, model)

main :: Control ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  -- show banner
  showBanner

  -- initialize main view
  (view, model) <- initializeMainView

  -- enter the main loop
  mainLoop (view, model)
