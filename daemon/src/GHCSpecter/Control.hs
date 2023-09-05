{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module GHCSpecter.Control
  ( mainLoop,
    main,
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Lens (Lens', (%~), (&), (.~), (^.), _2)
import Control.Monad (guard, void, when)
import Data.Foldable (traverse_)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel.Common.Types (DriverId)
import GHCSpecter.Channel.Inbound.Types
  ( ConsoleRequest (..),
    Request (..),
    SessionRequest (..),
  )
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.Control.DSL
  ( addToStage,
    asyncWork,
    getCurrentTime,
    getLastUpdatedUI,
    getScene,
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
    scrollDownConsoleToEnd,
    sendRequest,
    shouldUpdate,
  )
import GHCSpecter.Control.Types (Control)
import GHCSpecter.Data.Map (alterToKeyMap, emptyKeyMap, forwardLookup)
import GHCSpecter.Data.Timing.Types (TimingTable (..))
import GHCSpecter.Graphics.DSL
  ( EventMap,
    HitEvent (..),
    Scene (..),
    ViewPort (..),
    eventMapGlobalViewPort,
    eventMapId,
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    HasServerState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Constants
  ( WidgetConfig (..),
    timingHeight,
    timingMaxWidth,
    timingWidth,
    uiUpdateInterval,
  )
import GHCSpecter.UI.Types
  ( ConsoleUI (..),
    HasBlockerUI (..),
    HasConsoleUI (..),
    HasModuleGraphUI (..),
    HasSessionUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIModel (..),
    HasUIState (..),
    HasViewPortInfo (..),
    ModuleGraphUI (..),
    SourceViewUI (..),
    TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    BlockerEvent (..),
    BlockerModuleGraphEvent (..),
    ConsoleEvent (..),
    Event (..),
    KeyEvent (..),
    ModuleGraphEvent (..),
    MouseEvent (..),
    SessionEvent (..),
    SourceViewEvent (..),
    SpecialKey (KeyBackspace, KeyEnter),
    SubModuleEvent (..),
    SystemEvent (..),
    Tab (..),
    TimingEvent (..),
    UserEvent (..),
  )
import GHCSpecter.Util.Dump
  ( dumpMemory,
    dumpModGraph,
    dumpTiming,
  )
import GHCSpecter.Util.Transformation
  ( hitItem,
    isValid,
    transformScroll,
    transformZoom,
    translateToOrigin,
  )
import GHCSpecter.Worker.Timing
  ( timingBlockerGraphWorker,
    timingWorker,
  )
import System.IO (IOMode (..), withFile)

data HandlerHoverScrollZoom = HandlerHoverScrollZoom
  { handlerHover :: [(Text, Lens' UIModel (Maybe Text))],
    handlerScroll :: [(Text, Lens' UIModel ViewPortInfo)],
    handlerZoom :: [(Text, Lens' UIModel ViewPortInfo)]
  }

nextUserEvent :: (e ~ Event) => Control e UserEvent
nextUserEvent = do
  ev <- nextEvent
  case ev of
    SysEv (BkgEv bev) -> handleBackground bev >> nextUserEvent
    UsrEv uev -> pure uev

checkIfUpdatable :: Control e ()
checkIfUpdatable = do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime
  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

-- | showing ghc-specter banner in the beginning
-- TODO: this should be rewritten
showBanner :: Control e ()
showBanner = do
  startTime <- getCurrentTime
  ui <- getUI
  let ui' = ui {_uiModel = ui._uiModel {_modelTransientBanner = Just 0}}
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
              ui' = ui {_uiModel = ui._uiModel {_modelTransientBanner = Just r}}
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
          modifyUI $ \ui ->
            ui
              { _uiModel =
                  ui._uiModel
                    { _modelSourceView =
                        ui._uiModel._modelSourceView
                          { _srcViewFocusedBinding = Just sym
                          }
                    }
              }
  | msg == ":goto-source" = do
      modifyUISS $ \(ui, ss) ->
        let mmod = forwardLookup drvId (ss._serverDriverModuleMap)
            ui' =
              let model = ui._uiModel
                  model' =
                    model
                      { _modelSourceView =
                          model._modelSourceView
                            { _srcViewExpandedModule = mmod
                            },
                        _modelTabDestination = Just TabSourceView
                      }
               in ui {_uiModel = model'}
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
  (Double, Double) ->
  UIModel ->
  UIModel
scroll emap lensViewPort (dx, dy) model =
  let ViewPort (cx0, _) (cx1, _) = eventMapGlobalViewPort emap
      vp@(ViewPort (vx0, _) (vx1, _)) = model ^. lensViewPort ^. vpViewPort
      mvpExtent = sceneExtents emap
      scale = (cx1 - cx0) / (vx1 - vx0)
      vp' = transformScroll mvpExtent scale (dx, dy) vp
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
      -- the currently drawn (temporary) view.
      vp =
        fromMaybe (model ^. lensViewPort . vpViewPort) (model ^. lensViewPort . vpTempViewPort)
      rx = (x - cx0) / (cx1 - cx0)
      ry = (y - cy0) / (cy1 - cy0)
      vp' = (transformZoom (rx, ry) scale vp)
      vp'' = if isValid vp' then vp' else vp
   in (lensViewPort . vpTempViewPort .~ Just vp'') model

-- TODO: this function should handle all of MouseEvent.
handleHoverScrollZoom ::
  (e ~ Event) =>
  (UserEvent -> Maybe Text) ->
  HandlerHoverScrollZoom ->
  MouseEvent ->
  -- | returns whether event was handled
  Control e Bool
handleHoverScrollZoom hitWho handlers mev =
  case mev of
    MouseMove scene_id (x, y) -> do
      memap <- getScene scene_id
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
                    then pure ((uiModel . hoverLens .~ mnowHit) ui, ss {_serverShouldUpdate = False})
                    else Nothing
             in fromMaybe (ui, ss) mupdated
        let mprevHit = ui ^. uiModel . hoverLens
            mnowHit = ui' ^. uiModel . hoverLens
            isChanged = mnowHit /= mprevHit
        pure isChanged
    Scroll scene_id (_x, _y) (dx, dy) -> do
      memap <- getScene scene_id
      case memap of
        Nothing -> pure False
        Just emap -> do
          handleFor handlerScroll $ \(component, scrollLens) -> do
            let isHandled = eventMapId emap == component
            modifyUI $ \ui ->
              let mupdated = do
                    guard isHandled
                    pure $ (uiModel %~ scroll emap scrollLens (dx, dy)) ui
               in fromMaybe ui mupdated
            pure isHandled
    ZoomUpdate scene_id (xcenter, ycenter) scale -> do
      memap <- getScene scene_id
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

dumpWork :: (UIState -> ServerState -> Text) -> FilePath -> Control Event ()
dumpWork mkContent file = do
  ui <- getUI
  asyncWork $ \ssref -> do
    ss <- readTVarIO ssref
    let txt = mkContent ui ss
    withFile file WriteMode $ \h ->
      TIO.hPutStrLn h txt

handleConsole :: (e ~ Event) => ConsoleEvent DriverId -> Control e ()
handleConsole (ConsoleTab i) = do
  modifyUI $ \ui ->
    ui
      { _uiModel =
          ui._uiModel
            { _modelConsole =
                ui._uiModel._modelConsole
                  { _consoleFocus = Just i
                  }
            }
      }
  refresh
handleConsole (ConsoleKey key) = do
  model0 <- (._uiModel) <$> getUI
  if key == "Enter"
    then case model0._modelConsole._consoleFocus of
      Nothing -> pure ()
      Just drvId -> do
        let msg = model0._modelConsole._consoleInputEntry
        appendNewCommand drvId msg
        modifyUI $ \ui ->
          ui
            { _uiModel =
                ui._uiModel
                  { _modelConsole =
                      ui._uiModel._modelConsole
                        { _consoleInputEntry = ""
                        }
                  }
            }
        handleConsoleCommand drvId msg
        scrollDownConsoleToEnd
        refresh
    else pure ()
handleConsole (ConsoleInput content) = do
  modifyUI $ \ui ->
    ui
      { _uiModel =
          ui._uiModel
            { _modelConsole =
                ui._uiModel._modelConsole
                  { _consoleInputEntry = content
                  }
            }
      }
  refresh
handleConsole (ConsoleButtonPressed isImmediate msg) = do
  if isImmediate
    then do
      model0 <- (._uiModel) <$> getUI
      case model0._modelConsole._consoleFocus of
        Nothing -> pure ()
        Just drvId -> do
          appendNewCommand drvId msg
          modifyUI $ \ui ->
            ui
              { _uiModel =
                  ui._uiModel
                    { _modelConsole =
                        ui._uiModel._modelConsole
                          { _consoleInputEntry = ""
                          }
                    }
              }
          handleConsoleCommand drvId msg
          scrollDownConsoleToEnd
          refresh
    else do
      modifyUI $ \ui ->
        ui
          { _uiModel =
              ui._uiModel
                { _modelConsole =
                    ui._uiModel._modelConsole
                      { _consoleInputEntry = msg
                      }
                }
          }
      refresh
handleConsole ConsoleDumpTiming = do
  -- TODO: The dump file name should be customizable.
  printMsg "dumping timing information to dump-timing.svg"
  dumpWork dumpTiming "dump-timing.svg"
handleConsole ConsoleDumpMemory = do
  -- TODO: The dump file name should be customizable.
  printMsg "dumping memory alloc information to dump-memory.svg"
  dumpWork dumpMemory "dump-memory.svg"
handleConsole ConsoleDumpModGraph = do
  -- TODO: The dump file name should be customizable.
  printMsg "dumping module graph information to dump-modgraph.svg"
  dumpWork (\_ ss -> dumpModGraph ss) "dump-modgraph.svg"

-- TODO: this should be separated out with session type.
handleBackground :: (e ~ Event) => BackgroundEvent -> Control e ()
handleBackground MessageChanUpdated = do
  asyncWork timingWorker
  modifySS $ \ss -> ss {_serverShouldUpdate = True}
  refresh
handleBackground RefreshUI = refresh

-- | SessionEvent is common regardless of the selected tab
handleSessionEvent :: (e ~ Event) => SessionEvent -> Control e ()
handleSessionEvent sev = do
  case sev of
    SaveSessionEv -> do
      saveSession
      modifySS $ \ss -> ss {_serverShouldUpdate = False}
    ResumeSessionEv -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelConsole =
                          ui._uiModel._modelConsole
                            { _consoleFocus = Nothing
                            }
                      }
                }
            sinfo = ss._serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = False}
            ss' =
              ss
                { _serverSessionInfo = sinfo',
                  _serverPaused = emptyKeyMap,
                  _serverShouldUpdate = True
                }
         in (ui', ss')
      printMsg "sending SessionReq Resume to GHC"
      sendRequest (SessionReq Resume)
      refresh
    PauseSessionEv -> do
      modifySS $ \ss ->
        let sinfo = ss._serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = True}
         in ss
              { _serverSessionInfo = sinfo',
                _serverShouldUpdate = True
              }
      printMsg "sending SessionReq Pause to GHC"
      sendRequest (SessionReq Pause)
      refresh

goSession :: (e ~ Event) => UserEvent -> Control e ()
goSession ev = do
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\_ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = [],
              handlerScroll =
                [ ("module-status", modelSession . sessionUIModStatusViewPort),
                  ("session-process", modelSession . sessionUIProcessViewPort),
                  ("session-rts", modelSession . sessionUIRtsViewPort)
                ],
              handlerZoom =
                [("session-process", modelSession . sessionUIProcessViewPort)]
            }
          mev
    _ -> pure ()

goModuleGraph :: (e ~ Event) => UserEvent -> Control e ()
goModuleGraph ev = do
  case ev of
    MainModuleEv mev -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui._uiModel
            mgui = model._modelMainModuleGraph
            mgui' = handleModuleGraphEv mev mgui
            model' = model {_modelMainModuleGraph = mgui'}
            ui' = ui {_uiModel = model'}
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    SubModuleEv sev -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui._uiModel
            model' =
              case sev of
                SubModuleGraphEv sgev ->
                  let mgui = snd (model._modelSubModuleGraph)
                      mgui' = handleModuleGraphEv sgev mgui
                   in model
                        { _modelSubModuleGraph =
                            let (lvl, _) = model._modelSubModuleGraph
                             in (lvl, mgui')
                        }
                SubModuleLevelEv d' ->
                  model
                    { _modelSubModuleGraph =
                        let (_, gr) = model._modelSubModuleGraph
                         in (d', gr)
                    }
            ui' = ui {_uiModel = model'}
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          ( \case
              MainModuleEv (HoverOnModuleEv mmodu) -> mmodu
              SubModuleEv (SubModuleGraphEv (HoverOnModuleEv mmodu)) -> mmodu
              _ -> Nothing
          )
          HandlerHoverScrollZoom
            { handlerHover =
                [ ("main-module-graph", modelMainModuleGraph . modGraphUIHover),
                  ("sub-module-graph", modelSubModuleGraph . _2 . modGraphUIHover)
                ],
              handlerScroll =
                [ ("main-module-graph", modelMainModuleGraph . modGraphViewPort),
                  ("sub-module-graph", modelSubModuleGraph . _2 . modGraphViewPort)
                ],
              handlerZoom =
                [ ("main-module-graph", modelMainModuleGraph . modGraphViewPort),
                  ("sub-module-graph", modelSubModuleGraph . _2 . modGraphViewPort)
                ]
            }
          mev
    _ -> pure ()
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      ModuleGraphUI
    handleModuleGraphEv (HoverOnModuleEv mhovered) =
      \mgui -> mgui {_modGraphUIHover = mhovered}
    handleModuleGraphEv (ClickOnModuleEv mclicked) =
      \mgui -> mgui {_modGraphUIClick = mclicked}

goSourceView :: (e ~ Event) => UserEvent -> Control e ()
goSourceView ev = do
  case ev of
    SourceViewEv (SelectModule expandedModu') -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelSourceView =
                          ui._uiModel._modelSourceView
                            { _srcViewExpandedModule = Just expandedModu'
                            }
                      }
                }
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    SourceViewEv UnselectModule -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelSourceView =
                          ui._uiModel._modelSourceView
                            { _srcViewExpandedModule = Nothing
                            }
                      }
                }
            ss' = ss {_serverShouldUpdate = False}
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
      let bps = ss'._serverModuleBreakpoints
      sendRequest $ SessionReq (SetModuleBreakpoints bps)
      refresh
    SourceViewEv (SourceViewTab tab) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelSourceView =
                          ui._uiModel._modelSourceView
                            { _srcViewSuppViewTab = Just tab
                            }
                      }
                }
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\_ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = [],
              handlerScroll =
                [ ("module-tree", modelSourceView . srcViewModuleTreeViewPort),
                  ("source-view", modelSourceView . srcViewSourceViewPort),
                  ("supple-view-contents", modelSourceView . srcViewSuppViewPort)
                ],
              handlerZoom =
                [ ("source-view", modelSourceView . srcViewSourceViewPort),
                  ("supple-view-contents", modelSourceView . srcViewSuppViewPort)
                ]
            }
          mev
    _ -> pure ()

goTiming :: (e ~ Event) => UserEvent -> Control e ()
goTiming ev = do
  case ev of
    TimingEv ToCurrentTime -> do
      modifyUISS $ \(ui, ss) ->
        let model = ui._uiModel
            ttable =
              fromMaybe
                (ss._serverTiming._tsTimingTable)
                (model._modelTiming._timingFrozenTable)
            timingInfos = ttable._ttableTimingInfos
            -- TODO: this should be drawn from a library function.
            nMods = length timingInfos
            totalHeight = 5 * nMods
            model' =
              let timing_ui = model._modelTiming
                  timing_ui' =
                    timing_ui
                      { _timingUIViewPort =
                          ViewPortInfo
                            ( ViewPort
                                (timingMaxWidth - timingWidth, fromIntegral totalHeight - timingHeight)
                                (timingMaxWidth, fromIntegral totalHeight)
                            )
                            Nothing
                      }
               in model {_modelTiming = timing_ui'}
            ui' = ui {_uiModel = model'}
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    TimingEv (TimingFlow isFlowing) -> do
      printMsg $ "TimingFlow " <> T.pack (show isFlowing)
      modifyUISS $ \(ui, ss) ->
        let model = ui._uiModel
            ttable = ss._serverTiming._tsTimingTable
            timing_ui = model._modelTiming
            timing_ui'
              | isFlowing = timing_ui {_timingFrozenTable = Nothing}
              | otherwise = timing_ui {_timingFrozenTable = Just ttable}
            model' = model {_modelTiming = timing_ui'}
            ui' = ui {_uiModel = model'}
         in (ui', ss)
      refresh
    TimingEv (UpdatePartition b) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelTiming =
                          ui._uiModel._modelTiming
                            { _timingUIPartition = b
                            }
                      }
                }
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    TimingEv (UpdateParallel b) -> do
      modifyUISS $ \(ui, ss) ->
        let ui' =
              ui
                { _uiModel =
                    ui._uiModel
                      { _modelTiming =
                          ui._uiModel._modelTiming
                            { _timingUIHowParallel = b
                            }
                      }
                }
            ss' = ss {_serverShouldUpdate = False}
         in (ui', ss')
      refresh
    TimingEv (HoverOnModule modu) -> do
      modifyUI $ \ui ->
        ui
          { _uiModel =
              ui._uiModel
                { _modelTiming =
                    ui._uiModel._modelTiming
                      { _timingUIHoveredModule = Just modu
                      }
                }
          }
      refresh
    TimingEv (HoverOffModule _modu) -> do
      modifyUI $ \ui ->
        ui
          { _uiModel =
              ui._uiModel
                { _modelTiming =
                    ui._uiModel._modelTiming
                      { _timingUIHoveredModule = Nothing
                      }
                }
          }
      refresh
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\case TimingEv (HoverOnModule modu) -> Just modu; _ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = [("timing-chart", modelTiming . timingUIHoveredModule)],
              handlerScroll = [("timing-chart", modelTiming . timingUIViewPort)],
              handlerZoom = [("timing-chart", modelTiming . timingUIViewPort)]
            }
          mev
    _ -> pure ()

goBlocker :: (e ~ Event) => UserEvent -> Control e ()
goBlocker ev = do
  case ev of
    BlockerEv (ComputeBlockerGraph) -> do
      printMsg "compute blocker graph is pressed"
      asyncWork timingBlockerGraphWorker
      refresh
    BlockerEv (BlockerModuleGraphEv (BMGGraph e)) -> do
      printMsg ("blocker module graph event: " <> T.pack (show e))
      pure ()
    BlockerEv (BlockerModuleGraphEv (BMGUpdateLevel lvl)) -> do
      printMsg ("blocker module graph update: " <> T.pack (show lvl))
      modifySS $ \ss ->
        ss
          { _serverTiming =
              ss._serverTiming
                { _tsBlockerDetailLevel = lvl
                }
          }
      asyncWork timingBlockerGraphWorker
      refresh
    _ -> pure ()
  case ev of
    MouseEv mev ->
      void $
        handleHoverScrollZoom
          (\_ -> Nothing)
          HandlerHoverScrollZoom
            { handlerHover = [],
              handlerScroll =
                [ ("blocker-module-graph", modelBlocker . blockerUIViewPort)
                ],
              handlerZoom =
                [ ("blocker-module-graph", modelBlocker . blockerUIViewPort)
                ]
            }
          mev
    _ -> pure ()

initializeMainView :: Control e ()
initializeMainView =
  modifyUI $ \ui ->
    ui
      { _uiModel =
          ui._uiModel
            { _modelTransientBanner = Nothing
            }
      }

-- TODO: for now, we list all of the components. but later, it will be dynamic

stageFrame :: (e ~ Event) => Control e ()
stageFrame = do
  model <- (._uiModel) <$> getUI
  let wcfg = model._modelWidgetConfig
      allCfgs =
        (M.toList wcfg._wcfgTopLevel)
          ++ (M.toList wcfg._wcfgSession)
          ++ (M.toList wcfg._wcfgModuleGraph)
          ++ (M.toList wcfg._wcfgSourceView)
          ++ (M.toList wcfg._wcfgTiming)
      mkScene (name, gvp) =
        let lvp =
              case L.lookup name lensMap of
                Nothing -> translateToOrigin gvp
                Just l ->
                  let vpi = model ^. l
                   in fromMaybe (vpi._vpViewPort) (vpi._vpTempViewPort)
         in Scene
              { sceneId = name,
                sceneGlobalViewPort = gvp,
                sceneLocalViewPort = lvp,
                sceneElements = [],
                sceneExtents = Nothing
              }
      scenes = fmap mkScene allCfgs
  traverse_ addToStage scenes
  where
    lensMap :: [(Text, Lens' UIModel ViewPortInfo)]
    lensMap =
      [ ("console-main", modelConsole . consoleViewPort),
        ("module-status", modelSession . sessionUIModStatusViewPort),
        ("session-process", modelSession . sessionUIProcessViewPort),
        ("session-rts", modelSession . sessionUIRtsViewPort),
        ("main-module-graph", modelMainModuleGraph . modGraphViewPort),
        ("sub-module-graph", modelSubModuleGraph . _2 . modGraphViewPort),
        ("module-tree", modelSourceView . srcViewModuleTreeViewPort),
        ("source-view", modelSourceView . srcViewSourceViewPort),
        ("supple-view", modelSourceView . srcViewSuppViewPort),
        ("supple-view-contents", modelSourceView . srcViewSuppViewPort),
        ("timing-chart", modelTiming . timingUIViewPort),
        ("blocker-module-graph", modelBlocker . blockerUIViewPort)
      ]

-- | top-level main loop, branching according to tab event
mainLoop :: forall e r. (e ~ Event) => Control e r
mainLoop = do
  tab <- (._uiModel._modelTab) <$> getUI
  case tab of
    TabSession -> branchLoop goSession
    TabModuleGraph -> branchLoop goModuleGraph
    TabSourceView -> branchLoop goSourceView
    TabTiming -> branchLoop goTiming
    TabBlocker -> branchLoop goBlocker
  where
    branchLoop :: (UserEvent -> Control e ()) -> Control e r
    branchLoop go = loop
      where
        handleConsoleKey ev =
          case ev of
            KeyEv (NormalKeyPressed txt) -> do
              currInput <- (._uiModel._modelConsole._consoleInputEntry) <$> getUI
              pure $ ConsoleEv (ConsoleInput (currInput <> txt))
            KeyEv (SpecialKeyPressed KeyEnter) -> do
              -- TODO: should use enum, not text
              pure $ ConsoleEv (ConsoleKey "Enter")
            KeyEv (SpecialKeyPressed KeyBackspace) -> do
              currInput <- (._uiModel._modelConsole._consoleInputEntry) <$> getUI
              pure $ ConsoleEv (ConsoleInput (T.dropEnd 1 currInput))
            _ -> pure ev
        handleConsoleHoverScrollZoom ev =
          case ev of
            MouseEv mev -> do
              isHandled <-
                handleHoverScrollZoom
                  (\_ -> Nothing)
                  HandlerHoverScrollZoom
                    { handlerHover = [],
                      handlerScroll =
                        [ ("console-main", modelConsole . consoleViewPort)
                        ],
                      handlerZoom =
                        [ ("console-main", modelConsole . consoleViewPort)
                        ]
                    }
                  mev
              if isHandled
                then nextUserEvent
                else pure ev
            _ -> pure ev

        handleClick ev0 =
          case ev0 of
            MouseEv (MouseClick scene_id (x, y)) -> do
              memap <- getScene scene_id
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

        -- return: True -> go back to main loop, False -> local loop
        afterClick ev = do
          case ev of
            SessionEv sev -> handleSessionEvent sev >> pure True
            TabEv tab' -> do
              tab <- (._uiModel._modelTab) <$> getUI
              if (tab /= tab')
                then do
                  modifyUI $ \ui ->
                    let model = ui._uiModel
                        model' =
                          model
                            { _modelTab = tab',
                              _modelTabDestination = Nothing
                            }
                     in ui {_uiModel = model'}
                  refresh
                  pure True
                else pure False
            ConsoleEv cev ->
              handleConsole cev >> pure False
            _ -> go ev >> pure False

        loop :: Control Event r
        loop = do
          checkIfUpdatable
          printMsg "wait for the next event"
          ev0 <- nextUserEvent
          printMsg $ "event received: " <> T.pack (show ev0)
          -- handle console key press event
          ev1 <- handleConsoleKey ev0
          -- handle console hover/scroll/zoom
          ev2 <- handleConsoleHoverScrollZoom ev1
          -- handle click
          ev3 <- handleClick ev2
          toMainLoop <- afterClick ev3
          stageFrame
          if toMainLoop
            then mainLoop
            else loop

-- | top-level loop.
-- mainLoop :: forall e r. (e ~ Event) => Control e r
-- mainLoop = do
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
