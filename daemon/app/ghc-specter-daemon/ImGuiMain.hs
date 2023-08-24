{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ImGuiMain (uiMain) where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    readTVarIO,
    writeTQueue,
    writeTVar,
  )
import Control.Error.Util (note)
import Control.Monad (when)
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bits ((.|.))
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (nullPtr)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
import GHCSpecter.Data.Timing.Types
  ( TimingTable (..),
  )
import GHCSpecter.Data.Timing.Util (isModuleCompilationDone)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
  )
import GHCSpecter.Graphics.DSL
  ( EventMap,
    Primitive,
    Scene (..),
    ViewPort (..),
  )
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Components.GraphView qualified as GraphView
import GHCSpecter.UI.Components.TimingView qualified as TimingView
import GHCSpecter.UI.Constants (timingMaxWidth)
import GHCSpecter.UI.Types
  ( ModuleGraphUI (..),
    TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    SubModuleEvent (..),
    Tab (..),
    UserEvent (..),
  )
import Handler
  ( handleClick,
    handleMove,
    sendToControl,
  )
import ImGui
import ImGui.Enum (ImGuiMouseButton_ (..), ImGuiWindowFlags_ (..))
import ImGui.ImGuiIO.Implementation (imGuiIO_Fonts_get)
import Paths_ghc_specter_daemon (getDataDir)
import STD.Deletable (delete)
import System.FilePath ((</>))
import Text.Printf (printf)
import Util.GUI
  ( currentOrigin,
    finalize,
    globalCursorPosition,
    initialize,
    paintWindow,
    showFramerate,
  )
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    addEventMap,
    buildEventMap,
    renderScene,
    runImRender,
    toTab,
  )

windowFlagsScroll :: CInt
windowFlagsScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
      .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar

mkRenderState :: ReaderT (SharedState UserEvent) IO (ImRenderState UserEvent)
mkRenderState = do
  shared <- ask
  draw_list <- liftIO getWindowDrawList
  oxy <- liftIO currentOrigin
  pure
    ImRenderState
      { currSharedState = shared,
        currDrawList = draw_list,
        currOrigin = oxy
      }

renderMainModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMainModuleGraph ui ss = do
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let mgrui = ui._uiModel._modelMainModuleGraph

          mainModuleClicked = mgrui._modGraphUIClick
          mainModuleHovered = mgrui._modGraphUIHover

          scene :: Scene (Primitive UserEvent)
          scene =
            fmap MainModuleEv
              <$> ( runIdentity $
                      GraphView.buildModuleGraph nameMap valueFor grVisInfo (mainModuleClicked, mainModuleHovered)
                  )
          emap = buildEventMap scene
          (vx0, vy0) = scene.sceneLocalViewPort.topLeft
          (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
          totalW = vx1 - vx0
          totalH = vy1 - vy0
      renderState <- mkRenderState
      liftIO $ do
        runImRender renderState $ do
          renderScene scene
          addEventMap emap
          handleMove (totalW, totalH)
          handleClick (totalW, totalH)
        dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
        dummy dummy_sz
        delete dummy_sz
  where
    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    drvModMap = ss._serverDriverModuleMap
    mgs = ss._serverModuleGraphState
    clustering = mgs._mgsClustering
    timing = ss._serverTiming._tsTimingMap
    valueFor name =
      fromMaybe 0 $ do
        cluster <- L.lookup name clustering
        let nTot = length cluster
        if nTot == 0
          then Nothing
          else do
            let compiled = filter (isModuleCompilationDone drvModMap timing) cluster
                nCompiled = length compiled
            pure (fromIntegral nCompiled / fromIntegral nTot)

renderSubModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderSubModuleGraph ui ss = do
  case esubgraph of
    Left _err -> pure ()
    Right subgraph -> do
      let valueForSub name
            | isModuleCompilationDone drvModMap timing name = 1
            | otherwise = 0
          sceneSub :: Scene (Primitive UserEvent)
          sceneSub =
            fmap (SubModuleEv . SubModuleGraphEv)
              <$> ( runIdentity $
                      GraphView.buildModuleGraph nameMap valueForSub subgraph (mainModuleClicked, subModuleHovered)
                  )
          -- TODO: this should be set up from buildModuleGraph
          sceneSub' = sceneSub {sceneId = "sub-module-graph"}
          emap = buildEventMap sceneSub'
          (vx0, vy0) = sceneSub'.sceneLocalViewPort.topLeft
          (vx1, vy1) = sceneSub'.sceneLocalViewPort.bottomRight
          totalW = vx1 - vx0
          totalH = vy1 - vy0
      renderState <- mkRenderState
      liftIO $ do
        runImRender renderState $ do
          renderScene sceneSub'
          addEventMap emap
        -- handleMove (totalW, totalH)
        -- handleClick (totalW, totalH)
        dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
        dummy dummy_sz
        delete dummy_sz
  where
    mgrui = ui._uiModel._modelMainModuleGraph
    (detailLevel, sgrui) = ui._uiModel._modelSubModuleGraph

    mainModuleClicked = mgrui._modGraphUIClick
    subModuleHovered = sgrui._modGraphUIHover

    nameMap = ss._serverModuleGraphState._mgsModuleGraphInfo.mginfoModuleNameMap
    drvModMap = ss._serverDriverModuleMap
    mgs = ss._serverModuleGraphState
    subgraphs = mgs._mgsSubgraph

    timing = ss._serverTiming._tsTimingMap

    esubgraph :: Either String _
    esubgraph = do
      selected <-
        note "no module cluster is selected" mainModuleClicked
      subgraphsAtTheLevel <-
        note (printf "%s subgraph is not computed" (show detailLevel)) (L.lookup detailLevel subgraphs)
      subgraph <-
        note
          (printf "cannot find the subgraph for the module cluster %s" (T.unpack selected))
          (L.lookup selected subgraphsAtTheLevel)
      pure subgraph

renderTimingView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderTimingView ui ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      renderScene scene
      addEventMap emap
    -- handleMouseMove (totalW, totalH)
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
  where
    drvModMap = ss._serverDriverModuleMap
    tui = ui._uiModel._modelTiming
    ttable = ss._serverTiming._tsTimingTable
    timingInfos = ttable._ttableTimingInfos

    nMods = length timingInfos
    totalHeight = 5 * nMods
    vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

    tui' =
      tui
        { _timingUIPartition = True,
          _timingUIViewPort = ViewPortInfo vp Nothing
        }

    scene :: Scene (Primitive UserEvent)
    scene =
      ( fmap TimingEv
          <$> runIdentity (TimingView.buildTimingChart drvModMap tui' ttable)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

renderMemoryView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderMemoryView ui ss = do
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      renderScene scene
      addEventMap emap
    -- handleMouseMove (totalW, totalH)
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
  where
    drvModMap = ss._serverDriverModuleMap
    tui = ui._uiModel._modelTiming
    ttable = ss._serverTiming._tsTimingTable
    timingInfos = ttable._ttableTimingInfos

    nMods = length timingInfos
    totalHeight = 5 * nMods
    vp = ViewPort (0, 0) (timingMaxWidth, fromIntegral totalHeight)

    tui' =
      tui
        { _timingUIPartition = True,
          _timingUIViewPort = ViewPortInfo vp Nothing
        }

    scene :: Scene (Primitive UserEvent)
    scene = runIdentity $ TimingView.buildMemChart False 200 drvModMap tui' ttable
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

renderConsole :: ReaderT (SharedState UserEvent) IO ()
renderConsole = do
  chanQEv <- (.sharedChanQEv) <$> ask
  liftIO $ do
    -- Buttons return true when clicked (most widgets return true when edited/activated)
    whenM (toBool <$> button (":focus 1" :: CString)) $ do
      atomically $
        writeTQueue
          chanQEv
          (UsrEv (ConsoleEv (ConsoleTab (DriverId 1))))
    whenM (toBool <$> button (":next" :: CString)) $ do
      atomically $
        writeTQueue
          chanQEv
          (UsrEv (ConsoleEv (ConsoleButtonPressed True ":next")))

singleFrame ::
  ImGuiIO ->
  GLFWwindow ->
  UIState ->
  ServerState ->
  SharedState UserEvent ->
  IO (SharedState UserEvent)
singleFrame io window ui ss oldShared = do
  -- poll events for this frame
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  showFramerate io
  mxy <- globalCursorPosition

  -- initialize event map for this frame
  let emref = oldShared.sharedEventMap
  atomically $ writeTVar emref []
  isClicked <- toBool <$> isMouseClicked_ (fromIntegral (fromEnum ImGuiMouseButton_Left))
  let upd1
        | oldShared.sharedMousePos == mxy || isNothing mxy = \s -> s {sharedIsMouseMoved = False}
        | otherwise = \s -> s {sharedMousePos = mxy, sharedIsMouseMoved = True}
      upd2
        | isClicked = \s -> s {sharedIsClicked = True}
        | otherwise = \s -> s {sharedIsClicked = False}
      newShared = upd2 . upd1 $ oldShared

  newShared' <- flip runReaderT newShared $ do
    -- main canvas
    _ <- liftIO $ begin ("main" :: CString) nullPtr 0
    _ <- liftIO $ beginTabBar ("##TabBar" :: CString)
    zerovec <- liftIO $ newImVec2 0 0
    --
    -- main module graph tab
    bMainModGraph <- toBool <$> liftIO (beginTabItem ("main module graph" :: CString))
    when bMainModGraph $ do
      liftIO $ beginChild ("#main-modgraph" :: CString) zerovec (fromBool False) windowFlagsScroll
      renderMainModuleGraph ui ss
      liftIO endChild
      liftIO endTabItem
    -- sub module graph tab
    bSubModGraph <- toBool <$> liftIO (beginTabItem ("sub module graph" :: CString))
    when bSubModGraph $ do
      liftIO $ beginChild ("#sub-modgraph" :: CString) zerovec (fromBool False) windowFlagsScroll
      renderSubModuleGraph ui ss
      liftIO endChild
      liftIO endTabItem
    -- timing view tab
    bTimingView <- toBool <$> liftIO (beginTabItem ("timing view" :: CString))
    when bTimingView $ do
      liftIO $ beginChild ("#timing" :: CString) zerovec (fromBool False) windowFlagsScroll
      renderTimingView ui ss
      liftIO endChild
      liftIO endTabItem
    -- memory view tab
    bMemoryView <- toBool <$> liftIO (beginTabItem ("memory view" :: CString))
    when bMemoryView $ do
      liftIO $ beginChild ("#memory" :: CString) zerovec (fromBool False) windowFlagsScroll
      renderMemoryView ui ss
      liftIO endChild
      liftIO endTabItem
    --
    liftIO $ delete zerovec
    liftIO endTabBar
    liftIO end

    -- console window
    _ <- liftIO $ begin ("console" :: CString) nullPtr windowFlagsScroll
    renderConsole
    liftIO end

    -- post-rendering event handling: there are events discovered after rendering such as Tab.
    let tabState = (bMainModGraph, bSubModGraph, bTimingView, bMemoryView)
    when (newShared.sharedTabState /= tabState) $
      case toTab tabState of
        Nothing -> pure ()
        Just tab -> liftIO $ sendToControl newShared (TabEv tab)
    pure $ newShared {sharedTabState = tabState}

  --
  -- render call
  render
  --
  -- empty background with fill color
  paintWindow window (0.45, 0.55, 0.60 {- bluish gray -})
  -- stage the frame
  imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
  -- commit the frame
  glfwSwapBuffers window
  --
  pure newShared'

prepareAssets :: ImGuiIO -> IO (ImFont, ImFont)
prepareAssets io = do
  dir <- getDataDir
  let free_sans_path = dir </> "assets" </> "FreeSans.ttf"
      free_mono_path = dir </> "assets" </> "FreeMono.ttf"
  fonts <- imGuiIO_Fonts_get io
  -- _fontDefault <- imFontAtlas_AddFontDefault fonts
  _fontDefault <-
    withCString free_sans_path $ \cstr -> do
      imFontAtlas_AddFontFromFileTTF fonts cstr (8 * 2.0)
  fontSans <-
    withCString free_sans_path $ \cstr -> do
      imFontAtlas_AddFontFromFileTTF fonts cstr 8
  fontMono <-
    withCString free_mono_path $ \cstr ->
      imFontAtlas_AddFontFromFileTTF fonts cstr 8
  pure (fontSans, fontMono)

uiMain :: ServerSession -> ClientSession -> TVar [EventMap UserEvent] -> IO ()
uiMain servSess cliSess emref = do
  -- initialize window
  (ctxt, io, window) <- initialize
  -- prepare assets (fonts)
  (fontSans, fontMono) <- prepareAssets io

  -- state and event channel
  let uiref = cliSess._csUIStateRef
      ssref = servSess._ssServerStateRef
      chanQEv = cliSess._csPublisherEvent
      shared0 =
        SharedState
          { sharedMousePos = Nothing,
            sharedIsMouseMoved = False,
            sharedIsClicked = False,
            sharedTabState = (True, False, False, False),
            sharedChanQEv = chanQEv,
            sharedFontSans = fontSans,
            sharedFontMono = fontMono,
            sharedEventMap = emref
          }

  -- just start with module graph tab for now
  atomically $
    writeTQueue chanQEv (UsrEv (TabEv TabModuleGraph))

  -- main loop
  flip loopM shared0 $ \oldShared -> do
    ui <- readTVarIO uiref
    ss <- readTVarIO ssref
    newShared <- singleFrame io window ui ss oldShared
    -- loop is going on while the value from the following statement is True.
    willClose <- toBool <$> glfwWindowShouldClose window
    if willClose then pure (Right ()) else pure (Left newShared)

  -- close window
  finalize ctxt window
