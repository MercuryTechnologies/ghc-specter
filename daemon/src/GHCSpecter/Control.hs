{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GHCSpecter.Control (
  mainLoop,
  main,
) where

import Control.Lens (Lens', to, (%~), (&), (.~), (^.), _1, _2)
import Control.Monad (guard, void, when)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Channel.Inbound.Types (
  ConsoleRequest (..),
  Request (..),
  SessionRequest (..),
 )
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Control.Types (
  asyncWork,
  getCurrentTime,
  getLastUpdatedUI,
  getScene,
  getUI,
  hitScene,
  modifyAndReturn,
  modifyAndReturnBoth,
  modifySS,
  modifyUI,
  modifyUISS,
  nextEvent,
  printMsg,
  putUI,
  refresh,
  refreshUIAfter,
  saveSession,
  sendRequest,
  shouldUpdate,
  type Control,
 )
import GHCSpecter.Data.Map (alterToKeyMap, emptyKeyMap, forwardLookup)
import GHCSpecter.Data.Timing.Types (HasTimingTable (..))
import GHCSpecter.Graphics.DSL (
  EventMap,
  HitEvent (..),
  Scene (sceneExtents),
  ViewPort (..),
  eventMapGlobalViewPort,
  eventMapId,
  moveBoundingBoxBy,
 )
import GHCSpecter.Server.Types (
  ConsoleItem (..),
  HasServerState (..),
  HasTimingState (..),
 )
import GHCSpecter.UI.Constants (
  timingHeight,
  timingMaxWidth,
  timingWidth,
  uiUpdateInterval,
 )
import GHCSpecter.UI.Types (
  HasConsoleUI (..),
  HasModuleGraphUI (..),
  HasSessionUI (..),
  HasSourceViewUI (..),
  HasTimingUI (..),
  HasUIModel (..),
  HasUIState (..),
  HasViewPortInfo (..),
  ModuleGraphUI (..),
  UIModel,
  ViewPortInfo (..),
 )
import GHCSpecter.UI.Types.Event (
  BackgroundEvent (..),
  BlockerModuleGraphEvent (..),
  ConsoleEvent (..),
  Event (..),
  KeyEvent (..),
  ModuleGraphEvent (..),
  MouseEvent (..),
  ScrollDirection (..),
  SessionEvent (..),
  SourceViewEvent (..),
  SpecialKey (KeyBackspace, KeyEnter),
  SubModuleEvent (..),
  Tab (..),
  TimingEvent (..),
 )
import GHCSpecter.Util.Transformation (
  hitItem,
  isValid,
  transformScroll,
  transformZoom,
 )
import GHCSpecter.Worker.Timing (
  timingBlockerGraphWorker,
  timingWorker,
 )

data HandlerHoverScrollZoom = HandlerHoverScrollZoom
  { handlerHover :: [(Text, Lens' UIModel (Maybe Text))]
  , handlerScroll :: [(Text, Lens' UIModel ViewPortInfo)]
  , handlerZoom :: [(Text, Lens' UIModel ViewPortInfo)]
  }

updateLastUpdated :: Control e ()
updateLastUpdated = do
  now <- getCurrentTime
  modifyUI $ \ui ->
    if (ui ^. uiShouldUpdate)
      then ui & uiLastUpdated .~ now
      else ui

checkIfUpdatable :: Control e ()
checkIfUpdatable = do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime
  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

-- | showing ghc-specter banner in the beginning
showBanner :: Control e ()
showBanner = do
  startTime <- getCurrentTime
  ui <- getUI
  let ui' = (uiModel . modelTransientBanner .~ Just 0) ui
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
              ui' = (uiModel . modelTransientBanner .~ Just r) ui
          putUI ui'
          refreshUIAfter 0.1
          go start
        else pure ()

handleConsoleCommand :: (e ~ Event) => DriverId -> Text -> Control e ()
handleConsoleCommand drvId msg
  | msg == ":next" = sendRequest $ ConsoleReq drvId NextBreakpoint
  | msg == ":show-renamed" = sendRequest $ ConsoleReq drvId ShowRenamed
  | msg == ":show-expr" = sendRequest $ ConsoleReq drvId ShowExpr
  | msg == ":show-splice" = sendRequest $ ConsoleReq drvId ShowSplice
  | msg == ":show-result" = sendRequest $ ConsoleReq drvId ShowResult
  | msg == ":unqualified" = sendRequest $ ConsoleReq drvId ShowUnqualifiedImports
  | msg == ":list-core" = sendRequest $ ConsoleReq drvId ListCore
  | ":print-core" `T.isPrefixOf` msg = do
      let args = maybe [] NE.tail $ NE.nonEmpty (T.words msg)
      sendRequest $ ConsoleReq drvId (PrintCore args)
      case NE.nonEmpty args of
        Nothing -> pure ()
        Just (NE.head -> sym) ->
          modifyUI (uiModel . modelSourceView . srcViewFocusedBinding .~ Just sym)
  | msg == ":goto-source" = do
      modifyUISS $ \(ui, ss) ->
        let mmod = ss ^. serverDriverModuleMap . to (forwardLookup drvId)
            ui' =
              ui
                & (uiModel . modelSourceView . srcViewExpandedModule .~ mmod)
                  . (uiModel . modelTab .~ TabSourceView)
         in (ui', ss)
      -- TODO: this goes back to top-level. this should be taken out of this function's scope.
      refresh
      mainLoop
  | msg == ":dump-heap" = sendRequest $ ConsoleReq drvId DumpHeap
  | msg == ":exit-ghc-debug" = sendRequest $ SessionReq ExitGhcDebug
  | otherwise = sendRequest $ ConsoleReq drvId (Ping msg)

appendNewCommand :: DriverId -> Text -> Control e ()
appendNewCommand drvId newMsg = do
  modifySS $ \ss ->
    let newCmd = ConsoleCommand newMsg
        append Nothing = Just [newCmd]
        append (Just prevMsgs) = Just (prevMsgs ++ [newCmd])
        ss' = ss & (serverConsole %~ alterToKeyMap append drvId)
     in ss'

scroll ::
  EventMap e ->
  Lens' UIModel ViewPortInfo ->
  (ScrollDirection, (Double, Double)) ->
  UIModel ->
  UIModel
scroll emap lensViewPort (dir, (dx, dy)) model =
  let ViewPort (cx0, _) (cx1, _) = eventMapGlobalViewPort emap
      vp@(ViewPort (vx0, _) (vx1, _)) = model ^. lensViewPort ^. vpViewPort
      mvpExtent = sceneExtents emap
      scale = (cx1 - cx0) / (vx1 - vx0)
      vp' = transformScroll mvpExtent dir scale (dx, dy) vp
      vp'' = if isValid vp' then vp' else vp
   in (lensViewPort .~ ViewPortInfo vp'' Nothing) model

zoom ::
  EventMap e ->
  Lens' UIModel ViewPortInfo ->
  ((Double, Double), Double) ->
  UIModel ->
  UIModel
zoom emap lensViewPort ((x, y), scale) model =
  let ViewPort (cx0, cy0) (cx1, cy1) = eventMapGlobalViewPort emap
      -- NOTE: While zooming is in progress, the scaling is always relative to
      -- the last UI viewport, not relative to the currently drawn (temporary)
      -- view.
      vp = model ^. lensViewPort . vpViewPort
      rx = (x - cx0) / (cx1 - cx0)
      ry = (y - cy0) / (cy1 - cy0)
      vp' = (transformZoom (rx, ry) scale vp)
      vp'' = if isValid vp' then vp' else vp
   in (lensViewPort . vpTempViewPort .~ Just vp'') model

-- TODO: this function should handle MouseEvent.
handleHoverScrollZoom ::
  (e ~ Event) =>
  (Event -> Maybe Text) ->
  HandlerHoverScrollZoom ->
  MouseEvent ->
  -- | returns whether event was handled
  Control e Bool
handleHoverScrollZoom hitWho handlers mev =
  case mev of
    MouseMove (x, y) -> do
      memap <- hitScene (x, y)
      handleFor handlerHover $ \(component, hoverLens) -> do
        ((ui, _), (ui', _)) <-
          modifyAndReturnBoth $ \(ui, ss) ->
            let mupdated = do
                  emap <- memap
                  guard (eventMapId emap == component)
                  let mprevHit = ui ^. uiModel . hoverLens
                      mnowHit = do
                        hitEvent <- hitItem (x, y) emap
                        ev' <- hitEventHoverOn hitEvent
                        hitWho ev'
                  if mnowHit /= mprevHit
                    then pure ((uiModel . hoverLens .~ mnowHit) ui, (serverShouldUpdate .~ False) ss)
                    else Nothing
             in fromMaybe (ui, ss) mupdated
        let mprevHit = ui ^. uiModel . hoverLens
            mnowHit = ui' ^. uiModel . hoverLens
        pure (mnowHit /= mprevHit)
    Scroll dir' (x, y) (dx, dy) -> do
      memap <- hitScene (x, y)
      case memap of
        Nothing -> pure False
        Just emap -> do
          handleFor handlerScroll $ \(component, scrollLens) -> do
            let isHandled = eventMapId emap == component
            modifyUI $ \ui ->
              let mupdated = do
                    guard isHandled
                    pure $ (uiModel %~ scroll emap scrollLens (dir', (dx, dy))) ui
               in fromMaybe ui mupdated
            pure isHandled
    ZoomUpdate (xcenter, ycenter) scale -> do
      memap <- hitScene (xcenter, ycenter)
      case memap of
        Nothing -> pure False
        Just emap -> do
          handleFor handlerZoom $ \(component, zoomLens) -> do
            let isHandled = eventMapId emap == component
            modifyUI $ \ui ->
              let mupdated = do
                    guard isHandled
                    pure $ (uiModel %~ zoom emap zoomLens ((xcenter, ycenter), scale)) ui
               in fromMaybe ui mupdated
            pure isHandled
    -- TODO: this should have pointer info.
    ZoomEnd -> do
      _ <-
        handleFor handlerZoom $ \(_component, zoomLens) -> do
          modifyUI $ \ui ->
            let ui' = case ui ^. uiModel . zoomLens . vpTempViewPort of
                  Just viewPort -> (uiModel . zoomLens .~ ViewPortInfo viewPort Nothing) ui
                  Nothing -> ui
             in ui'
          -- TODO: This is because it cannot distinguish components unfortunately.
          pure False
      refresh
      pure False
    MouseClick {} -> pure False
    MouseDown {} -> pure False
    MouseUp {} -> pure False
  where
    handleFor ::
      (HandlerHoverScrollZoom -> [(Text, Lens' UIModel a)]) ->
      ((Text, Lens' UIModel a) -> Control e Bool) ->
      Control e Bool
    handleFor getHandler go = do
      rs <- traverse go (getHandler handlers)
      let isHandled = or rs
      when isHandled refresh
      pure isHandled

scrollDownConsoleToEnd :: Control Event ()
scrollDownConsoleToEnd = do
  mext <- fmap (sceneExtents =<<) (getScene "console-main")
  for_ mext $ \(ViewPort (x0, _y0) (_x1, y1)) -> do
    modifyUI $ \ui ->
      let ViewPortInfo (vp'@(ViewPort (x0', _y0') (_x1', y1'))) _ = ui ^. uiModel . modelConsole . consoleViewPort
          vp'' = moveBoundingBoxBy (x0 - x0', y1 - y1') vp'
       in (uiModel . modelConsole . consoleViewPort .~ ViewPortInfo vp'' Nothing) ui

handleConsole :: (e ~ Event) => ConsoleEvent DriverId -> Control e ()
handleConsole (ConsoleTab i) = do
  modifyUI (uiModel . modelConsole . consoleFocus .~ Just i)
  refresh
handleConsole (ConsoleKey key) = do
  model0 <- (^. uiModel) <$> getUI
  if key == "Enter"
    then case model0 ^. modelConsole . consoleFocus of
      Nothing -> pure ()
      Just drvId -> do
        let msg = model0 ^. modelConsole . consoleInputEntry
        appendNewCommand drvId msg
        modifyUI (uiModel . modelConsole . consoleInputEntry .~ "")
        handleConsoleCommand drvId msg
        scrollDownConsoleToEnd
        refresh
    else pure ()
handleConsole (ConsoleInput content) = do
  modifyUI (uiModel . modelConsole . consoleInputEntry .~ content)
  refresh
handleConsole (ConsoleButtonPressed isImmediate msg) = do
  if isImmediate
    then do
      model0 <- (^. uiModel) <$> getUI
      case model0 ^. modelConsole . consoleFocus of
        Nothing -> pure ()
        Just drvId -> do
          appendNewCommand drvId msg
          modifyUI (uiModel . modelConsole . consoleInputEntry .~ "")
          handleConsoleCommand drvId msg
          scrollDownConsoleToEnd
          refresh
    else do
      modifyUI (uiModel . modelConsole . consoleInputEntry .~ msg)
      refresh

-- TODO: this should be separated out with session type.
handleBackground :: (e ~ Event) => BackgroundEvent -> Control e ()
handleBackground MessageChanUpdated = do
  asyncWork timingWorker
  modifySS (serverShouldUpdate .~ True)
  refresh
handleBackground RefreshUI = refresh

goSession :: (e ~ Event) => Event -> Control e ()
goSession ev = do
  case ev of
    SessionEv SaveSessionEv -> do
      saveSession
      modifySS (serverShouldUpdate .~ False)
    SessionEv ResumeSessionEv -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                & (uiModel . modelConsole . consoleFocus .~ Nothing)
            sinfo = ss ^. serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = False}
            ss' =
              (serverSessionInfo .~ sinfo')
                . (serverPaused .~ emptyKeyMap)
                . (serverShouldUpdate .~ True)
                $ ss
         in (ui', ss')
      sendRequest (SessionReq Resume)
      refresh
    SessionEv PauseSessionEv -> do
      modifySS $ \ss ->
        let sinfo = ss ^. serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = True}
         in ss
              & (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True)
      sendRequest (SessionReq Pause)
      refresh
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\_ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = []
            , handlerScroll =
                [ ("module-status", modelSession . sessionUIModStatusViewPort)
                , ("session-process", modelSession . sessionUIProcessViewPort)
                , ("session-rts", modelSession . sessionUIRtsViewPort)
                ]
            , handlerZoom =
                [("session-process", modelSession . sessionUIProcessViewPort)]
            }
          mev
    _ -> pure ()

goModuleGraph :: (e ~ Event) => Event -> Control e ()
goModuleGraph ev = do
  case ev of
    MainModuleEv mev -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui ^. uiModel
            mgui = model ^. modelMainModuleGraph
            mgui' = handleModuleGraphEv mev mgui
            model' = (modelMainModuleGraph .~ mgui') model
            ui' = (uiModel .~ model') ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    SubModuleEv sev -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui ^. uiModel
            model' =
              case sev of
                SubModuleGraphEv sgev ->
                  let mgui = model ^. modelSubModuleGraph . _2
                      mgui' = handleModuleGraphEv sgev mgui
                   in (modelSubModuleGraph . _2 .~ mgui') model
                SubModuleLevelEv d' ->
                  (modelSubModuleGraph . _1 .~ d') model
            ui' = (uiModel .~ model') ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\case MainModuleEv (HoverOnModuleEv mmodu) -> mmodu; _ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover =
                [ ("main-module-graph", modelMainModuleGraph . modGraphUIHover)
                ]
            , handlerScroll =
                [ ("main-module-graph", modelMainModuleGraph . modGraphViewPort)
                , ("sub-module-graph", modelSubModuleGraph . _2 . modGraphViewPort)
                ]
            , handlerZoom =
                [ ("main-module-graph", modelMainModuleGraph . modGraphViewPort)
                , ("sub-module-graph", modelSubModuleGraph . _2 . modGraphViewPort)
                ]
            }
          mev
    _ -> pure ()
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      ModuleGraphUI
    handleModuleGraphEv (HoverOnModuleEv mhovered) = (modGraphUIHover .~ mhovered)
    handleModuleGraphEv (ClickOnModuleEv mclicked) = (modGraphUIClick .~ mclicked)

goSourceView :: (e ~ Event) => Event -> Control e ()
goSourceView ev = do
  case ev of
    SourceViewEv (SelectModule expandedModu') -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelSourceView . srcViewExpandedModule .~ Just expandedModu') ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    SourceViewEv UnselectModule -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelSourceView . srcViewExpandedModule .~ Nothing) ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    SourceViewEv (SetBreakpoint modu isSet) -> do
      (_, ss') <-
        modifyAndReturn $ \(ui, ss) ->
          let updater
                | isSet = (modu :)
                | otherwise = L.delete modu
              ss' = (serverModuleBreakpoints %~ updater) ss
           in (ui, ss')
      let bps = ss' ^. serverModuleBreakpoints
      sendRequest $ SessionReq (SetModuleBreakpoints bps)
      refresh
    SourceViewEv (SourceViewTab tab) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelSourceView . srcViewSuppViewTab .~ Just tab) ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\_ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = []
            , handlerScroll =
                [ ("module-tree", modelSourceView . srcViewModuleTreeViewPort)
                , ("source-view", modelSourceView . srcViewSourceViewPort)
                , ("supple-view-contents", modelSourceView . srcViewSuppViewPort)
                ]
            , handlerZoom =
                [ ("source-view", modelSourceView . srcViewSourceViewPort)
                , ("supple-view-contents", modelSourceView . srcViewSuppViewPort)
                ]
            }
          mev
    _ -> pure ()

goTiming :: (e ~ Event) => Event -> Control e ()
goTiming ev = do
  case ev of
    TimingEv ToCurrentTime -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui ^. uiModel
            ttable =
              fromMaybe
                (ss ^. serverTiming . tsTimingTable)
                (model ^. modelTiming . timingFrozenTable)
            timingInfos = ttable ^. ttableTimingInfos
            -- TODO: this should be drawn from a library function.
            nMods = length timingInfos
            totalHeight = 5 * nMods
            model' =
              model
                & ( modelTiming . timingUIViewPort
                      .~ ViewPortInfo
                        ( ViewPort
                            (timingMaxWidth - timingWidth, fromIntegral totalHeight - timingHeight)
                            (timingMaxWidth, fromIntegral totalHeight)
                        )
                        Nothing
                  )
            ui' = (uiModel .~ model') ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    TimingEv (TimingFlow isFlowing) -> do
      printMsg $ "TimingFlow " <> T.pack (show isFlowing)
      modifyUISS $ \(ui, ss) ->
        let model = ui ^. uiModel
            ttable = ss ^. serverTiming . tsTimingTable
            model'
              | isFlowing = (modelTiming . timingFrozenTable .~ Nothing) model
              | otherwise = (modelTiming . timingFrozenTable .~ Just ttable) model
            ui' = (uiModel .~ model') ui
         in (ui', ss)
      refresh
    TimingEv (UpdatePartition b) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelTiming . timingUIPartition .~ b) ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    TimingEv (UpdateParallel b) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelTiming . timingUIHowParallel .~ b) ui
            ss' = (serverShouldUpdate .~ False) ss
         in (ui', ss')
      refresh
    TimingEv (HoverOnModule modu) -> do
      modifyUI (uiModel . modelTiming . timingUIHoveredModule .~ Just modu)
      refresh
    TimingEv (HoverOffModule _modu) -> do
      modifyUI (uiModel . modelTiming . timingUIHoveredModule .~ Nothing)
      refresh
    TimingEv ShowBlockerGraph -> do
      printMsg "show blocker graph is pressed"
      asyncWork timingBlockerGraphWorker
      modifyUI (uiModel . modelTiming . timingUIBlockerGraph .~ True)
      refresh
    TimingEv CloseBlockerGraph -> do
      printMsg "close blocker graph is pressed"
      modifyUI (uiModel . modelTiming . timingUIBlockerGraph .~ False)
      refresh
    TimingEv (BlockerModuleGraphEv (BMGGraph e)) -> do
      printMsg ("blocker module graph event: " <> T.pack (show e))
      pure ()
    TimingEv (BlockerModuleGraphEv (BMGUpdateLevel lvl)) -> do
      printMsg ("blocker module graph update: " <> T.pack (show lvl))
      modifySS (serverTiming . tsBlockerDetailLevel .~ lvl)
      asyncWork timingBlockerGraphWorker
      refresh
    _ -> pure ()
  case ev of
    MouseEv (MouseDown (Just (x, y))) -> do
      modifyUI (uiModel . modelTiming . timingUIHandleMouseMove .~ True)
      ui' <- getUI
      let vpi = ui' ^. uiModel . modelTiming . timingUIViewPort
          vp = fromMaybe (vpi ^. vpViewPort) (vpi ^. vpTempViewPort)
      onDraggingInTimingView (x, y) vp
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\case TimingEv (HoverOnModule modu) -> Just modu; _ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = [("timing-chart", modelTiming . timingUIHoveredModule)]
            , handlerScroll = [("timing-chart", modelTiming . timingUIViewPort)]
            , handlerZoom = [("timing-chart", modelTiming . timingUIViewPort)]
            }
          mev
    _ -> pure ()
  where
    addDelta :: (Double, Double) -> (Double, Double) -> ViewPort -> UIModel -> UIModel
    addDelta (x, y) (x', y') (ViewPort (tx, ty) (tx1, ty1)) =
      let (dx, dy) = (x' - x, y' - y)
       in modelTiming . timingUIViewPort
            .~ ViewPortInfo (ViewPort (tx - dx, ty - dy) (tx1 - dx, ty1 - dy)) Nothing

    -- (x, y): mouse down point, vp: viewport
    onDraggingInTimingView (x, y) vp = do
      checkIfUpdatable
      ev' <- nextEvent
      case ev' of
        MouseEv (MouseUp (Just (x', y'))) ->
          modifyUI $
            -- turn off mouse move event handling
            (uiModel . modelTiming . timingUIHandleMouseMove .~ False)
              . (uiModel %~ addDelta (x, y) (x', y') vp)
        MouseEv (MouseMove (x', y')) -> do
          modifyUI $
            (uiModel %~ addDelta (x, y) (x', y') vp)
          updateLastUpdated
          onDraggingInTimingView (x, y) vp
        _ -> onDraggingInTimingView (x, y) vp

initializeMainView :: Control e ()
initializeMainView =
  modifyUI (uiModel . modelTransientBanner .~ Nothing)

-- | top-level loop, branching according to tab event
mainLoop :: forall e r. (e ~ Event) => Control e r
mainLoop = do
  tab <- (^. uiModel . modelTab) <$> getUI
  case tab of
    TabSession -> branchLoop goSession
    TabModuleGraph -> branchLoop goModuleGraph
    TabSourceView -> branchLoop goSourceView
    TabTiming -> branchLoop goTiming
  where
    branchLoop :: (Event -> Control e ()) -> Control e r
    branchLoop go = loop
      where
        handleConsoleKey ev =
          case ev of
            KeyEv (NormalKeyPressed txt) -> do
              currInput <- (^. uiModel . modelConsole . consoleInputEntry) <$> getUI
              pure $ ConsoleEv (ConsoleInput (currInput <> txt))
            KeyEv (SpecialKeyPressed KeyEnter) -> do
              -- TODO: should use enum, not text
              pure $ ConsoleEv (ConsoleKey "Enter")
            KeyEv (SpecialKeyPressed KeyBackspace) -> do
              currInput <- (^. uiModel . modelConsole . consoleInputEntry) <$> getUI
              pure $ ConsoleEv (ConsoleInput (T.dropEnd 1 currInput))
            _ -> pure ev
        handleConsoleHoverScrollZoom ev =
          case ev of
            MouseEv mev -> do
              isHandled <-
                handleHoverScrollZoom
                  (\_ -> Nothing)
                  HandlerHoverScrollZoom
                    { handlerHover = []
                    , handlerScroll =
                        [ ("console-main", modelConsole . consoleViewPort)
                        ]
                    , handlerZoom =
                        [ ("console-main", modelConsole . consoleViewPort)
                        ]
                    }
                  mev
              if isHandled
                then nextEvent
                else pure ev
            _ -> pure ev

        handleClick ev0 =
          case ev0 of
            MouseEv (MouseClick (x, y)) -> do
              memap <- hitScene (x, y)
              let ev' =
                    case memap of
                      Just emap ->
                        let mev = do
                              hitEvent <- hitItem (x, y) emap
                              Right ev_ <- hitEventClick hitEvent
                              pure ev_
                         in case mev of
                              Nothing -> ev0
                              Just ev_ -> ev_
                      _ -> ev0
              pure ev'
            _ -> pure ev0

        afterClick ev = do
          case ev of
            TabEv tab' -> do
              tab <- (^. uiModel . modelTab) <$> getUI
              if (tab /= tab')
                then do
                  modifyUI (uiModel . modelTab .~ tab')
                  refresh
                  mainLoop
                else loop
            ConsoleEv cev -> handleConsole cev >> loop
            BkgEv bev -> handleBackground bev >> loop
            _ -> go ev >> loop

        loop :: Control Event r
        loop = do
          checkIfUpdatable
          printMsg "wait for the next event"
          ev0 <- nextEvent
          printMsg $ "event received: " <> T.pack (show ev0)
          -- handle console key press event
          ev1 <- handleConsoleKey ev0
          -- handle console hover/scroll/zoom
          ev2 <- handleConsoleHoverScrollZoom ev1
          -- handle click
          ev3 <- handleClick ev2
          afterClick ev3

main :: (e ~ Event) => Control e ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  -- show banner
  showBanner

  -- initialize main view
  initializeMainView

  -- enter the main loop
  mainLoop
