{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GHCSpecter.Control (
  mainLoop,
  main,
) where

import Control.Lens (Lens', to, (%~), (&), (.~), (^.), _1, _2)
import Control.Monad (guard, when)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock qualified as Clock
import Data.Traversable (for)
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
  getUI,
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
  EventMap (..),
  HitEvent (..),
  ViewPort (..),
 )
import GHCSpecter.Server.Types (
  ConsoleItem (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState,
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
  HasUIViewRaw (..),
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
  ModuleGraphEvent (..),
  MouseEvent (..),
  ScrollDirection (..),
  SessionEvent (..),
  SourceViewEvent (..),
  SubModuleEvent (..),
  Tab (..),
  TimingEvent (..),
 )
import GHCSpecter.Util.Transformation (
  hitItem,
  hitScene,
  transformScroll,
  transformZoom,
 )
import GHCSpecter.Worker.Timing (
  timingBlockerGraphWorker,
  timingWorker,
 )
import Text.Read (readMaybe)

data HandlerHoverScrollZoom = HandlerHoverScrollZoom
  { handlerHover :: [(Text, Lens' UIModel (Maybe Text))]
  , handlerScroll :: [(Text, Lens' UIModel ViewPortInfo)]
  , handlerZoom :: [(Text, Lens' UIModel ViewPortInfo)]
  }

-- TODO: this function does almost nothing. remove.
defaultUpdateModel ::
  Event ->
  ServerState ->
  ServerState
defaultUpdateModel topEv =
  case topEv of
    TabEv _tab' ->
      serverShouldUpdate .~ False
    BkgEv MessageChanUpdated ->
      serverShouldUpdate .~ True
    _ ->
      id

updateLastUpdated :: Control ()
updateLastUpdated = do
  now <- getCurrentTime
  modifyUI $ \ui ->
    if (ui ^. uiShouldUpdate)
      then ui & uiLastUpdated .~ now
      else ui

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
  let ui' = (uiViewRaw . uiTransientBanner .~ Just 0) ui
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
              ui' = (uiViewRaw . uiTransientBanner .~ Just r) ui
          putUI ui'
          refreshUIAfter 0.1
          go start
        else pure ()

handleConsoleCommand :: DriverId -> Text -> Control ()
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
      mainLoop
  | msg == ":dump-heap" = sendRequest $ ConsoleReq drvId DumpHeap
  | msg == ":exit-ghc-debug" = sendRequest $ SessionReq ExitGhcDebug
  | otherwise = sendRequest $ ConsoleReq drvId (Ping msg)

appendNewCommand :: DriverId -> Text -> Control ()
appendNewCommand drvId newMsg = do
  modifySS $ \ss ->
    let newCmd = ConsoleCommand newMsg
        append Nothing = Just [newCmd]
        append (Just prevMsgs) = Just (prevMsgs ++ [newCmd])
        ss' = ss & (serverConsole %~ alterToKeyMap append drvId)
     in ss'

scroll :: EventMap -> Lens' UIModel ViewPortInfo -> (ScrollDirection, (Double, Double)) -> UIModel -> UIModel
scroll emap lensViewPort (dir, (dx, dy)) model =
  let ViewPort (cx0, _) (cx1, _) = eventMapGlobalViewPort emap
      vp@(ViewPort (vx0, _) (vx1, _)) = model ^. lensViewPort ^. vpViewPort
      scale = (cx1 - cx0) / (vx1 - vx0)
      vp' = transformScroll dir scale (dx, dy) vp
   in (lensViewPort .~ ViewPortInfo vp' Nothing) model

zoom :: EventMap -> Lens' UIModel ViewPortInfo -> ((Double, Double), Double) -> UIModel -> UIModel
zoom emap lensViewPort ((x, y), scale) model =
  let ViewPort (cx0, cy0) (cx1, cy1) = eventMapGlobalViewPort emap
      -- NOTE: While zooming is in progress, the scaling is always relative to
      -- the last UI viewport, not relative to the currently drawn (temporary)
      -- view.
      vp = model ^. lensViewPort . vpViewPort
      rx = (x - cx0) / (cx1 - cx0)
      ry = (y - cy0) / (cy1 - cy0)
      vp' = (transformZoom (rx, ry) scale vp)
   in (lensViewPort . vpTempViewPort .~ Just vp') model

goHoverScrollZoom ::
  HandlerHoverScrollZoom ->
  Event ->
  Control ()
goHoverScrollZoom handlers ev = do
  case ev of
    MouseEv (MouseMove (x, y)) -> do
      rs <-
        for (handlerHover handlers) $ \(component, hoverLens) -> do
          ((ui, _), (ui', _)) <-
            modifyAndReturnBoth $ \(ui, ss) ->
              let emaps = ui ^. uiViewRaw . uiRawEventMap
                  mupdated = do
                    emap <- hitScene (x, y) emaps
                    guard (eventMapId emap == component)
                    let mprevHit = ui ^. uiModel . hoverLens
                        mnowHit = hitEventHover =<< hitItem (x, y) emap
                    if mnowHit /= mprevHit
                      then pure ((uiModel . hoverLens .~ mnowHit) ui, (serverShouldUpdate .~ False) ss)
                      else Nothing
               in fromMaybe (ui, ss) mupdated
          let mprevHit = ui ^. uiModel . hoverLens
              mnowHit = ui' ^. uiModel . hoverLens
          pure (mnowHit /= mprevHit)
      when (or rs) refresh
    MouseEv (Scroll dir' (x, y) (dx, dy)) -> do
      for_ (handlerScroll handlers) $ \(component, scrollLens) ->
        modifyUI $ \ui ->
          let emaps = ui ^. uiViewRaw . uiRawEventMap
              mupdated = do
                emap <- hitScene (x, y) emaps
                guard (eventMapId emap == component)
                pure $ (uiModel %~ scroll emap scrollLens (dir', (dx, dy))) ui
           in fromMaybe ui mupdated
      refresh
    MouseEv (ZoomUpdate (xcenter, ycenter) scale) -> do
      for_ (handlerZoom handlers) $ \(component, zoomLens) ->
        modifyUI $ \ui ->
          let emaps = ui ^. uiViewRaw . uiRawEventMap
              mupdated = do
                emap <- hitScene (xcenter, ycenter) emaps
                guard (eventMapId emap == component)
                pure $ (uiModel %~ zoom emap zoomLens ((xcenter, ycenter), scale)) ui
           in fromMaybe ui mupdated
      refresh
    MouseEv ZoomEnd -> do
      for_ (handlerZoom handlers) $ \(_component, zoomLens) ->
        modifyUI $ \ui ->
          let ui' = case ui ^. uiModel . zoomLens . vpTempViewPort of
                Just viewPort -> (uiModel . zoomLens .~ ViewPortInfo viewPort Nothing) ui
                Nothing -> ui
           in ui'
      refresh
    _ -> pure ()

-- NOTE: This function should not exist forever.
goCommon :: Event -> Control ()
goCommon ev = do
  case ev of
    ConsoleEv (ConsoleTab i) -> do
      printMsg ("console tab: " <> T.pack (show i))
      modifyUI (uiModel . modelConsole . consoleFocus .~ Just i)
    ConsoleEv (ConsoleKey key) -> do
      model0 <- (^. uiModel) <$> getUI
      if key == "Enter"
        then case model0 ^. modelConsole . consoleFocus of
          Nothing -> pure ()
          Just drvId -> do
            let msg = model0 ^. modelConsole . consoleInputEntry
            appendNewCommand drvId msg
            modifyUI (uiModel . modelConsole . consoleInputEntry .~ "")
            handleConsoleCommand drvId msg
        else pure ()
    ConsoleEv (ConsoleInput content) -> do
      modifyUI (uiModel . modelConsole . consoleInputEntry .~ content)
    ConsoleEv (ConsoleButtonPressed isImmediate msg) -> do
      printMsg msg
      if isImmediate
        then do
          model0 <- (^. uiModel) <$> getUI
          case model0 ^. modelConsole . consoleFocus of
            Nothing -> pure ()
            Just drvId -> do
              appendNewCommand drvId msg
              modifyUI (uiModel . modelConsole . consoleInputEntry .~ "")
              handleConsoleCommand drvId msg
              refresh
        else do
          modifyUI (uiModel . modelConsole . consoleInputEntry .~ msg)
          refresh
    _ -> pure ()
  modifySS $ \ss -> defaultUpdateModel ev ss
  case ev of
    BkgEv MessageChanUpdated -> asyncWork timingWorker >> refresh
    BkgEv RefreshUI -> refresh
    _ -> pure ()

goSession :: Event -> Control ()
goSession ev = do
  case ev of
    SessionEv SaveSessionEv -> do
      saveSession
      modifySS (serverShouldUpdate .~ False)
    SessionEv ResumeSessionEv -> do
      modifyUISS $ \(ui, ss) ->
        let ui' = (uiModel . modelConsole . consoleFocus .~ Nothing) ui
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
    MouseEv (MouseClick (x, y)) -> do
      -- TODO: we need to make the whole Control transactional in updating the state
      ui0 <- getUI
      let emaps0 = ui0 ^. uiViewRaw . uiRawEventMap
          mhit0 = do
            emap0 <- L.find (\m -> eventMapId m == "session-button") emaps0
            hitEvent <- hitItem (x, y) emap0
            eventMsg <- snd (hitEventClick hitEvent)
            pure eventMsg
      case mhit0 of
        Just hit ->
          if
              | hit == "ResumeSession" -> do
                  modifyUISS $ \(ui, ss) ->
                    let ui' = (uiModel . modelConsole . consoleFocus .~ Nothing) ui
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
              | hit == "PauseSession" -> do
                  modifySS $ \ss ->
                    let sinfo = ss ^. serverSessionInfo
                        sinfo' = sinfo {sessionIsPaused = True}
                     in ss
                          & (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True)
                  sendRequest (SessionReq Pause)
                  refresh
              | otherwise -> pure ()
        _ -> pure ()
    _ -> pure ()
  goHoverScrollZoom
    HandlerHoverScrollZoom
      { handlerHover = []
      , handlerScroll =
          [ ("module-status", modelSession . sessionUIModStatusViewPort)
          , ("session-main", modelSession . sessionUIMainViewPort)
          ]
      , handlerZoom =
          [("session-main", modelSession . sessionUIMainViewPort)]
      }
    ev
  goCommon ev

goModuleGraph :: Event -> Control ()
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
    MouseEv (MouseClick (x, y)) -> do
      ((ui, _), (ui', _)) <-
        modifyAndReturnBoth $ \(ui, ss) ->
          let model = ui ^. uiModel
              emaps = ui ^. uiViewRaw . uiRawEventMap
              mprevHit = ui ^. uiModel . modelMainModuleGraph . modGraphUIClick
              mnowHit = do
                emap <- L.find (\m -> eventMapId m == "main-module-graph") emaps
                hitEvent <- hitItem (x, y) emap
                click <- snd $ hitEventClick hitEvent
                pure click
              (ui', ss')
                | mnowHit /= mprevHit =
                    let mev = ClickOnModuleEv mnowHit
                        mgui = model ^. modelMainModuleGraph
                        mgui' = handleModuleGraphEv mev mgui
                        model' = (modelMainModuleGraph .~ mgui') model
                     in ((uiModel .~ model') ui, (serverShouldUpdate .~ False) ss)
                | otherwise = (ui, ss)
           in (ui', ss')
      let mprevHit = ui ^. uiModel . modelMainModuleGraph . modGraphUIClick
          mnowHit = ui' ^. uiModel . modelMainModuleGraph . modGraphUIClick
      when (mnowHit /= mprevHit) refresh
    _ -> pure ()
  goHoverScrollZoom
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
    ev
  goCommon ev
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      ModuleGraphUI
    handleModuleGraphEv (HoverOnModuleEv mhovered) = (modGraphUIHover .~ mhovered)
    handleModuleGraphEv (ClickOnModuleEv mclicked) = (modGraphUIClick .~ mclicked)

goSourceView :: Event -> Control ()
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
    MouseEv (MouseClick (x, y)) -> do
      -- TODO: we need to make the whole Control transactional in updating the state
      ui0 <- getUI
      let memap0 = hitScene (x, y) (ui0 ^. uiViewRaw . uiRawEventMap)
          mscene = eventMapId <$> memap0
      printMsg (T.pack (show mscene))
      modifyUISS $ \(ui, ss) ->
        let emaps = ui ^. uiViewRaw . uiRawEventMap
         in case hitScene (x, y) emaps of
              Just emap ->
                if
                    | eventMapId emap == "module-tree" ->
                        let mnowHit = hitEventClick <$> hitItem (x, y) emap
                         in case mnowHit of
                              Just (wasActive, Just nowHit)
                                | not wasActive ->
                                    let ui' = (uiModel . modelSourceView . srcViewExpandedModule .~ Just nowHit) ui
                                        ss' = (serverShouldUpdate .~ False) ss
                                     in (ui', ss')
                                | otherwise ->
                                    let ui' = (uiModel . modelSourceView . srcViewExpandedModule .~ Nothing) ui
                                        ss' = (serverShouldUpdate .~ False) ss
                                     in (ui', ss')
                              _ -> (ui, ss)
                    | eventMapId emap == "supple-view-tab" ->
                        -- TODO: This text parsing should be eliminated after implementing typed events.
                        let mhitTab = do
                              hitEvent <- hitItem (x, y) emap
                              eventMsg <- snd (hitEventClick hitEvent)
                              readMaybe (T.unpack eventMsg)
                            ui' = (uiModel . modelSourceView . srcViewSuppViewTab .~ mhitTab) ui
                         in (ui', ss)
                    | otherwise -> (ui, ss)
              Nothing -> (ui, ss)
      refresh
    _ -> pure ()
  goHoverScrollZoom
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
    ev
  goCommon ev

goTiming :: Event -> Control ()
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
  goHoverScrollZoom
    HandlerHoverScrollZoom
      { handlerHover = [("timing-chart", modelTiming . timingUIHoveredModule)]
      , handlerScroll = [("timing-chart", modelTiming . timingUIViewPort)]
      , handlerZoom = [("timing-chart", modelTiming . timingUIViewPort)]
      }
    ev
  goCommon ev
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

initializeMainView :: Control ()
initializeMainView =
  modifyUI (uiViewRaw . uiTransientBanner .~ Nothing)

-- | top-level loop, branching according to tab event
mainLoop :: Control ()
mainLoop = do
  tab <- (^. uiModel . modelTab) <$> getUI
  case tab of
    TabSession -> branchLoop goSession
    TabModuleGraph -> branchLoop goModuleGraph
    TabSourceView -> branchLoop goSourceView
    TabTiming -> branchLoop goTiming
  where
    branchLoop go = loop
      where
        loop = do
          checkIfUpdatable
          printMsg "wait for the next event"
          ev <- nextEvent
          printMsg $ "event received: " <> T.pack (show ev)
          case ev of
            TabEv tab' -> do
              tab <- (^. uiModel . modelTab) <$> getUI
              if (tab /= tab')
                then do
                  modifyUI (uiModel . modelTab .~ tab')
                  refresh
                  mainLoop
                else loop
            MouseEv (MouseClick (x, y)) -> do
              emaps <- (^. uiViewRaw . uiRawEventMap) <$> getUI
              let mhitTab = do
                    emap <- L.find (\m -> eventMapId m == "tab") emaps
                    hitEvent <- hitItem (x, y) emap
                    eventMsg <- snd (hitEventClick hitEvent)
                    pure eventMsg
              let mtab' = case mhitTab of
                    Just "TabSession" -> Just TabSession
                    Just "TabModuleGraph" -> Just TabModuleGraph
                    Just "TabSourceView" -> Just TabSourceView
                    Just "TabTiming" -> Just TabTiming
                    _ -> Nothing
              tab <- (^. uiModel . modelTab) <$> getUI
              case mtab' of
                Nothing ->
                  -- Handling downstream
                  go ev >> loop
                Just tab' ->
                  if (tab /= tab')
                    then do
                      modifyUI (uiModel . modelTab .~ tab')
                      refresh
                      mainLoop
                    else loop
            _ ->
              go ev >> loop

main :: Control ()
main = do
  clientSessionStartTime <- getCurrentTime
  printMsg $ "client session starts at " <> T.pack (show clientSessionStartTime)

  -- show banner
  showBanner

  -- initialize main view
  initializeMainView

  -- enter the main loop
  mainLoop
