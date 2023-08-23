{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ImGuiMain (uiMain) where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    newTVarIO,
    readTVarIO,
    writeTQueue,
  )
import Control.Monad (when)
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isNothing)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (toBool)
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
  ( TimingUI (..),
    UIModel (..),
    UIState (..),
    ViewPortInfo (..),
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    UserEvent (..),
  )
import GHCSpecter.Util.Transformation qualified as Transformation
import ImGui
import ImGui.Enum (ImGuiWindowFlags_ (..))
import ImGui.ImGuiIO.Implementation (imGuiIO_Fonts_get)
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import Paths_ghc_specter_daemon (getDataDir)
import STD.Deletable (delete)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
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
  ( ImRender (..),
    ImRenderState (..),
    SharedState (..),
    buildEventMap,
    renderScene,
    runImRender,
  )

windowFlagsScroll :: CInt
windowFlagsScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
      .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar

mkRenderState :: ReaderT SharedState IO (ImRenderState e)
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

detectMouseMove :: (Show e) => (Double, Double) -> String -> EventMap e -> ImRender e ()
detectMouseMove (totalW, totalH) msg emap = do
  renderState <- ImRender ask
  when (renderState.currSharedState.sharedIsMouseMoved) $ do
    case renderState.currSharedState.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
        when (x' >= ox && x' <= ox + totalW && y' >= oy && y' <= oy + totalH) $
          liftIO $ do
            i <- readIORef hackVar
            modifyIORef' hackVar (+ 1)
            let xy = (x' - ox, y' - oy)
                mitem = Transformation.hitItem xy emap
            putStrLn $
              printf "[%d: %s], mouse (%.2f, %.2f) moved" i msg (x' - ox) (y' - oy)
            case mitem of
              Nothing -> putStrLn "Hit nothing."
              Just item -> do putStrLn "Hit something!" >> print item

showModuleGraph :: ServerState -> ReaderT SharedState IO ()
showModuleGraph ss = do
  shared <- ask
  _ <- liftIO $ begin ("module graph" :: CString) nullPtr windowFlagsScroll
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let scene :: Scene (Primitive Event)
          scene =
            fmap (UsrEv . MainModuleEv)
              <$> ( runIdentity $
                      GraphView.buildModuleGraph nameMap valueFor grVisInfo (Nothing, Nothing)
                  )
          emap = buildEventMap scene
          (vx0, vy0) = scene.sceneLocalViewPort.topLeft
          (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
          totalW = vx1 - vx0
          totalH = vy1 - vy0
      renderState <- mkRenderState
      liftIO $ do
        runImRender renderState $ do
          detectMouseMove (totalW, totalH) "modgraph" emap
          renderScene scene
        dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
        dummy dummy_sz
        delete dummy_sz
  liftIO end
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

showTimingView :: UIState -> ServerState -> ReaderT SharedState IO ()
showTimingView ui ss = do
  shared <- ask
  _ <- liftIO $ begin ("timing view" :: CString) nullPtr windowFlagsScroll
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      detectMouseMove (totalW, totalH) "timingview" emap
      renderScene scene
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
    end
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

    scene :: Scene (Primitive Event)
    scene =
      ( fmap (UsrEv . TimingEv)
          <$> runIdentity (TimingView.buildTimingChart drvModMap tui' ttable)
      )
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

showMemoryView :: UIState -> ServerState -> ReaderT SharedState IO ()
showMemoryView ui ss = do
  shared <- ask
  _ <- liftIO $ begin ("memory view" :: CString) nullPtr windowFlagsScroll
  renderState <- mkRenderState
  liftIO $ do
    runImRender renderState $ do
      detectMouseMove (totalW, totalH) "memoryview" emap
      renderScene scene
    dummy_sz <- newImVec2 (realToFrac totalW) (realToFrac totalH)
    dummy dummy_sz
    delete dummy_sz
    end
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

    scene :: Scene (Primitive Event)
    scene = runIdentity $ TimingView.buildMemChart False 200 drvModMap tui' ttable
    emap = buildEventMap scene

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = vx1 - vx0
    totalH = vy1 - vy0

showConsole :: ReaderT SharedState IO ()
showConsole = do
  chanQEv <- (.sharedChanQEv) <$> ask
  liftIO $ do
    _ <- begin ("ghc-specter console" :: CString) nullPtr 0
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
    end

{-# NOINLINE hackVar #-}
hackVar :: IORef Int
hackVar = unsafePerformIO (newIORef 0)

singleFrame ::
  ImGuiIO ->
  GLFWwindow ->
  UIState ->
  ServerState ->
  SharedState ->
  IO SharedState
singleFrame io window ui ss oldShared = do
  -- poll events for this frame
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  showFramerate io
  mxy <- globalCursorPosition
  newShared <-
    if oldShared.sharedMousePos == mxy || isNothing mxy
      then pure oldShared {sharedIsMouseMoved = False}
      else do
        -- v <- readIORef hackVar
        -- putStrLn (printf "%d: fire mouse move event!" v)
        -- modifyIORef' hackVar (+ 1)
        pure $ oldShared {sharedMousePos = mxy, sharedIsMouseMoved = True}

  flip runReaderT newShared $ do
    -- module graph window
    showModuleGraph ss
    -- timing view window
    showTimingView ui ss
    -- memory view window
    showMemoryView ui ss
    -- console window
    showConsole
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

  pure newShared

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

uiMain :: ServerSession -> ClientSession -> IO ()
uiMain servSess cliSess = do
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
            sharedChanQEv = chanQEv,
            sharedFontSans = fontSans,
            sharedFontMono = fontMono
          }

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
