{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module GHCSpecter.Control
  ( main,
  )
where

import Control.Lens (to, (%~), (&), (.~), (^.), _1, _2)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel.Inbound.Types
  ( ConsoleRequest (..),
    Request (..),
    SessionRequest (..),
  )
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Control.Types
  ( getCurrentTime,
    getLastUpdatedUI,
    getSS,
    getUI,
    nextEvent,
    printMsg,
    putSS,
    putUI,
    refreshUIAfter,
    saveSession,
    sendRequest,
    shouldUpdate,
    type Control,
  )
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState,
  )
import GHCSpecter.UI.Constants
  ( timingHeight,
    timingMaxWidth,
    timingWidth,
    uiUpdateInterval,
  )
import GHCSpecter.UI.Types
  ( HasConsoleUI (..),
    HasMainView (..),
    HasModuleGraphUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIModel (..),
    HasUIState (..),
    MainView,
    ModuleGraphUI (..),
    UIModel,
    UIState,
    UIView (..),
    emptyMainView,
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    ComponentTag (..),
    ConsoleEvent (..),
    Event (..),
    ModuleGraphEvent (..),
    MouseEvent (..),
    SessionEvent (..),
    SourceViewEvent (..),
    SubModuleEvent (..),
    Tab (..),
    TimingEvent (..),
  )
import GHCSpecter.Util.Map (forwardLookup)
import GHCSpecter.Util.Timing (makeTimingTable)

defaultUpdateModel ::
  Event ->
  (UIModel, ServerState) ->
  Control (UIModel, ServerState)
defaultUpdateModel topEv (oldModel, oldSS) =
  case topEv of
    TabEv _tab' -> do
      let newSS = (serverShouldUpdate .~ False) oldSS
      pure (oldModel, newSS)
    SourceViewEv (SelectModule expandedModu') -> do
      let newModel =
            (modelSourceView . srcViewExpandedModule .~ Just expandedModu') oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    SourceViewEv UnselectModule -> do
      let newModel =
            (modelSourceView . srcViewExpandedModule .~ Nothing) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    SourceViewEv (SetBreakpoint modu isSet) -> do
      let updater
            | isSet = (modu :)
            | otherwise = L.delete modu
          newSS = (serverModuleBreakpoints %~ updater) oldSS
          bps = newSS ^. serverModuleBreakpoints
      sendRequest $ SessionReq (SetModuleBreakpoints bps)
      pure (oldModel, newSS)
    MainModuleEv ev -> do
      let mgui = oldModel ^. modelMainModuleGraph
          mgui' = handleModuleGraphEv ev mgui
          newModel = (modelMainModuleGraph .~ mgui') oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    SubModuleEv sev ->
      case sev of
        SubModuleGraphEv ev -> do
          let mgui = oldModel ^. modelSubModuleGraph . _2
              mgui' = handleModuleGraphEv ev mgui
              newModel = (modelSubModuleGraph . _2 .~ mgui') oldModel
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newModel, newSS)
        SubModuleLevelEv d' -> do
          let newModel = (modelSubModuleGraph . _1 .~ d') oldModel
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newModel, newSS)
    SessionEv SaveSessionEv -> do
      saveSession
      let newSS = (serverShouldUpdate .~ False) oldSS
      pure (oldModel, newSS)
    SessionEv ResumeSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = False}
          newSS =
            (serverSessionInfo .~ sinfo')
              . (serverShouldUpdate .~ True)
              $ oldSS
          newModel = (modelConsole . consoleFocus .~ Nothing) oldModel
      sendRequest (SessionReq Resume)
      pure (newModel, newSS)
    SessionEv PauseSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = True}
          newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
      sendRequest (SessionReq Pause)
      pure (oldModel, newSS)
    TimingEv ToCurrentTime -> do
      let -- TODO: This is inefficient. Make a cache for timing table.
          timingInfos = makeTimingTable oldSS
          nMods = length timingInfos
          totalHeight = 5 * nMods
          newModel =
            oldModel
              & ( modelTiming . timingUIViewPortTopLeft
                    .~ ( fromIntegral (timingMaxWidth - timingWidth)
                       , fromIntegral (totalHeight - timingHeight)
                       )
                )
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    TimingEv (UpdatePartition b) -> do
      let newModel = (modelTiming . timingUIPartition .~ b) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    TimingEv (UpdateParallel b) -> do
      let newModel = (modelTiming . timingUIHowParallel .~ b) oldModel
          newSS = (serverShouldUpdate .~ False) oldSS
      pure (newModel, newSS)
    BkgEv MessageChanUpdated -> do
      let newSS = (serverShouldUpdate .~ True) oldSS
      pure (oldModel, newSS)
    BkgEv RefreshUI -> do
      pure (oldModel, oldSS)
    _ -> pure (oldModel, oldSS)
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      ModuleGraphUI
    handleModuleGraphEv (HoverOnModuleEv mhovered) = (modGraphUIHover .~ mhovered)
    handleModuleGraphEv (ClickOnModuleEv mclicked) = (modGraphUIClick .~ mclicked)

updateLastUpdated :: UIState -> Control UIState
updateLastUpdated ui = do
  now <- getCurrentTime
  let ui'
        | ui ^. uiShouldUpdate = ui & uiLastUpdated .~ now
        | otherwise = ui
  pure ui'

checkIfUpdatable :: Control ()
checkIfUpdatable = do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime
  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

-- | showing ghc-specter banner in the beginning
showBanner :: Control ()
showBanner = do
  startTime <- getCurrentTime
  ui <- getUI
  let ui' = (uiView .~ BannerMode 0) ui
  putUI ui'
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
          ui <- getUI
          let r = realToFrac (diff / duration)
              ui' = (uiView .~ BannerMode r) ui
          putUI ui'
          refreshUIAfter 0.1
          go start
        else pure ()

-- NOTE: This function should not exist forever.
goCommon :: Event -> (MainView, UIModel) -> Control (MainView, UIModel)
goCommon ev (view, model0) = do
  model <-
    case ev of
      ConsoleEv (ConsoleTab i) -> do
        printMsg ("console tab: " <> T.pack (show i))
        pure (model0 & modelConsole . consoleFocus .~ Just i)
      ConsoleEv (ConsoleKey key) ->
        if key == "Enter"
          then case model0 ^. modelConsole . consoleFocus of
            Nothing -> pure model0
            Just drvId -> do
              let msg = model0 ^. modelConsole . consoleInputEntry
                  model = (modelConsole . consoleInputEntry .~ "") model0
              model' <-
                if
                    | msg == ":next" -> do
                        sendRequest $ ConsoleReq drvId NextBreakpoint
                        pure model
                    | msg == ":unqualified" -> do
                        sendRequest $ ConsoleReq drvId ShowUnqualifiedImports
                        pure model
                    | msg == ":test" -> do
                        sendRequest $ ConsoleReq drvId Test
                        pure model
                    | msg == ":list-core" -> do
                        sendRequest $ ConsoleReq drvId ListCore
                        pure model
                    | ":print-core" `T.isPrefixOf` msg -> do
                        let args = maybe [] NE.tail $ NE.nonEmpty (T.words msg)
                        sendRequest $ ConsoleReq drvId (PrintCore args)
                        case NE.nonEmpty args of
                          Nothing -> pure model
                          Just (NE.head -> sym) ->
                            let model1 = (modelConsole . consoleLastBinding .~ Just sym) model
                             in pure model1
                    | msg == ":goto-source" -> do
                        ss <- getSS
                        let mmod = ss ^. serverDriverModuleMap . to (forwardLookup drvId)
                            model' =
                              (modelSourceView . srcViewExpandedModule .~ mmod) model
                        branchTab TabSourceView (view, model')
                        -- should not be reached.
                        pure model'
                    | otherwise -> do
                        sendRequest $ ConsoleReq drvId (Ping msg)
                        pure model
              pure model'
          else pure model0
      ConsoleEv (ConsoleInput content) -> do
        let model = (modelConsole . consoleInputEntry .~ content) model0
        pure model
      ConsoleEv (ConsoleButtonPressed content) -> do
        printMsg content
        let model = (modelConsole . consoleInputEntry .~ content) model0
        pure model
      _ -> pure model0
  ui <- getUI
  ss <- getSS
  (model', ss') <- defaultUpdateModel ev (model, ss)
  let -- just placeholder
      view' = view
      ui' =
        ui
          & (uiView .~ MainMode view')
            . (uiModel .~ model')
  putUI ui'
  putSS ss'
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
      MouseEv TimingView (MouseDown (Just (x, y))) -> do
        let (tx, ty) = model0 ^. modelTiming . timingUIViewPortTopLeft
            -- turn on mouse move event handling
            model1 = (modelTiming . timingUIHandleMouseMove .~ True) model0
        ui0 <- getUI
        let ui1 = ui0 & (uiModel .~ model1)
        putUI ui1
        onDraggingInTimingView model1 (x, y) (tx, ty)
      _ -> pure model0
  goCommon ev (view, model)
  where
    addDelta :: (Double, Double) -> (Double, Double) -> (Double, Double) -> UIModel -> UIModel
    addDelta (x, y) (x', y') (tx, ty) =
      let (dx, dy) = (x' - x, y' - y)
       in modelTiming . timingUIViewPortTopLeft .~ (tx - dx, ty - dy)

    -- (x, y): mouse down point, (tx, ty): viewport origin
    onDraggingInTimingView model_ (x, y) (tx, ty) = do
      checkIfUpdatable
      ev' <- nextEvent
      case ev' of
        MouseEv TimingView (MouseUp (Just (x', y'))) -> do
          let model =
                -- turn off mouse move event handling
                (modelTiming . timingUIHandleMouseMove .~ False)
                  . addDelta (x, y) (x', y') (tx, ty)
                  $ model_
          pure model
        MouseEv TimingView (MouseMove (Just (x', y'))) -> do
          let model = addDelta (x, y) (x', y') (tx, ty) model_
          ui0 <- getUI
          let ui1 = ui0 & (uiModel .~ model)
          ui <- updateLastUpdated ui1
          putUI ui
          onDraggingInTimingView model (x, y) (tx, ty)
        _ -> onDraggingInTimingView model_ (x, y) (tx, ty)

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
  ui1 <- getUI
  let ui1' = (uiView .~ MainMode emptyMainView) ui1
  putUI ui1'
  pure (emptyMainView, ui1' ^. uiModel)

-- | main loop
mainLoop :: (MainView, UIModel) -> Control ()
mainLoop (view, model) = branchTab (view ^. mainTab) (view, model)

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
