{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ImGuiMain (uiMain) where

import Control.Concurrent.STM
  ( TQueue,
    atomically,
    readTVarIO,
    writeTQueue,
  )
import Control.Monad.Extra (whenM, whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
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
  ( Scene (..),
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
import ImGui
import ImGui.Enum (ImGuiWindowFlags_ (..))
import ImGui.ImGuiIO.Implementation (imGuiIO_Fonts_get)
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import Paths_ghc_specter_daemon (getDataDir)
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.GUI
  ( finalize,
    initialize,
    paintWindow,
    showFramerate,
  )
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    renderPrimitive,
    runImRender,
  )

windowFlagsScroll :: CInt
windowFlagsScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
      .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar

showModuleGraph :: (ImFont, ImFont) -> ServerState -> ReaderT SharedState IO ()
showModuleGraph (fontSans, fontMono) ss = liftIO $ do
  _ <- begin ("module graph" :: CString) nullPtr windowFlagsScroll
  case mgs._mgsClusterGraph of
    Nothing -> pure ()
    Just grVisInfo -> do
      let scene =
            runIdentity $
              GraphView.buildModuleGraph nameMap valueFor grVisInfo (Nothing, Nothing)
          elems = sceneElements scene
          (vx0, vy0) = scene.sceneLocalViewPort.topLeft
          (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
          totalW = realToFrac (vx1 - vx0)
          totalH = realToFrac (vy1 - vy0)
      draw_list <- getWindowDrawList
      p <- getCursorScreenPos
      px <- imVec2_x_get p
      py <- imVec2_y_get p
      let renderState =
            ImRenderState
              { currSharedState = SharedState Nothing,
                currDrawList = draw_list,
                currOrigin = (px, py),
                currFontSans = fontSans,
                currFontMono = fontMono
              }
      runImRender renderState $ do
        traverse_ renderPrimitive elems
      dummy_sz <- newImVec2 totalW totalH
      dummy dummy_sz
      delete dummy_sz

      -- mouse event handling
      mouse_pos <- getMousePos
      -- mouse position is regarded as integer to reduce noise.
      mouse_x <- floor @_ @Int <$> imVec2_x_get mouse_pos
      mouse_y <- floor @_ @Int <$> imVec2_y_get mouse_pos
      putStrLn $
        "mouse (x, y) = " <> show (mouse_x, mouse_y)
      delete mouse_pos

  end
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

showTimingView :: (ImFont, ImFont) -> UIState -> ServerState -> ReaderT SharedState IO ()
showTimingView (fontSans, fontMono) ui ss = liftIO $ do
  _ <- begin ("timing view" :: CString) nullPtr windowFlagsScroll
  draw_list <- getWindowDrawList
  p <- getCursorScreenPos
  px <- imVec2_x_get p
  py <- imVec2_y_get p
  let renderState =
        ImRenderState
          { currSharedState = SharedState Nothing,
            currDrawList = draw_list,
            currOrigin = (px, py),
            currFontSans = fontSans,
            currFontMono = fontMono
          }
  runImRender renderState $ do
    traverse_ renderPrimitive elems
  dummy_sz <- newImVec2 totalW totalH
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

    scene = runIdentity $ TimingView.buildTimingChart drvModMap tui' ttable
    elems = scene.sceneElements

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = realToFrac (vx1 - vx0)
    totalH = realToFrac (vy1 - vy0)

showMemoryView :: (ImFont, ImFont) -> UIState -> ServerState -> ReaderT SharedState IO ()
showMemoryView (fontSans, fontMono) ui ss = liftIO $ do
  _ <- begin ("memory view" :: CString) nullPtr windowFlagsScroll
  draw_list <- getWindowDrawList
  p <- getCursorScreenPos
  px <- imVec2_x_get p
  py <- imVec2_y_get p
  let renderState =
        ImRenderState
          { currSharedState = SharedState Nothing,
            currDrawList = draw_list,
            currOrigin = (px, py),
            currFontSans = fontSans,
            currFontMono = fontMono
          }
  runImRender renderState $ do
    traverse_ renderPrimitive elems
  dummy_sz <- newImVec2 totalW totalH
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

    scene = runIdentity $ TimingView.buildMemChart False 200 drvModMap tui' ttable
    elems = scene.sceneElements

    (vx0, vy0) = scene.sceneLocalViewPort.topLeft
    (vx1, vy1) = scene.sceneLocalViewPort.bottomRight
    totalW = realToFrac (vx1 - vx0)
    totalH = realToFrac (vy1 - vy0)

showConsole :: TQueue Event -> ReaderT SharedState IO ()
showConsole chanQEv = liftIO $ do
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

singleFrame ::
  ImGuiIO ->
  (ImFont, ImFont) ->
  GLFWwindow ->
  UIState ->
  ServerState ->
  TQueue Event ->
  IO ()
singleFrame io (fontSans, fontMono) window ui ss chanQEv = do
  -- poll events for this frame
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  showFramerate io
  flip runReaderT (SharedState Nothing) $ do
    -- module graph window
    showModuleGraph (fontSans, fontMono) ss
    -- timing view window
    showTimingView (fontSans, fontMono) ui ss
    -- memory view window
    showMemoryView (fontSans, fontMono) ui ss
    -- console window
    showConsole chanQEv

  -- render call
  render

  -- empty background with fill color
  paintWindow window (0.45, 0.55, 0.60 {- bluish gray -})
  -- stage the frame
  imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
  -- commit the frame
  glfwSwapBuffers window

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
  assets <- prepareAssets io

  -- state and event channel
  let uiref = cliSess._csUIStateRef
      ssref = servSess._ssServerStateRef
      chanQEv = cliSess._csPublisherEvent

  -- main loop
  whileM $ do
    ui <- readTVarIO uiref
    ss <- readTVarIO ssref
    singleFrame io assets window ui ss chanQEv
    -- loop is going on while the value from the following statement is True.
    not . toBool <$> glfwWindowShouldClose window

  -- close window
  finalize ctxt window
