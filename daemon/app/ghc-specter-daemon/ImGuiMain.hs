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
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types (ModuleGraphInfo (..))
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
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    UserEvent (..),
  )
import ImGui
import ImGui.Enum (ImGuiWindowFlags_ (..))
import ImGui.ImGuiIO.Implementation (imGuiIO_Fonts_get)
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import Paths_ghc_specter_daemon (getDataDir)
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.GUI
  ( finalize,
    initialize,
    showFramerate,
  )
import Util.Render
  ( ImRenderState (..),
    renderPrimitive,
    runImRender,
  )

showModuleGraph :: (ImFont, ImFont) -> ServerState -> IO ()
showModuleGraph (fontSans, fontMono) ss = do
  let wflag =
        fromIntegral $
          fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
            .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar
  _ <- begin ("module graph" :: CString) nullPtr wflag
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
              { currDrawList = draw_list,
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

showConsole :: TQueue Event -> IO ()
showConsole chanQEv = do
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

drawBackground :: GLFWwindow -> IO ()
drawBackground window = do
  clear_color <- newImVec4 0.45 0.55 0.60 1.00
  alloca $ \p_dispW ->
    alloca $ \p_dispH -> do
      glfwGetFramebufferSize window p_dispW p_dispH
      dispW <- peek p_dispW
      dispH <- peek p_dispH
      glViewport 0 0 dispW dispH
      x <- imVec4_x_get clear_color
      y <- imVec4_y_get clear_color
      z <- imVec4_z_get clear_color
      w <- imVec4_w_get clear_color
      glClearColor (x * w) (y * w) (z * w) w
      glClear 0x4000 {- GL_COLOR_BUFFER_BIT -}
  delete clear_color

singleFrame ::
  ImGuiIO ->
  (ImFont, ImFont) ->
  GLFWwindow ->
  ServerState ->
  TQueue Event ->
  IO ()
singleFrame io (fontSans, fontMono) window ss chanQEv = do
  -- poll events for this frame
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  -- main drawing part
  showFramerate io
  -- module graph window
  showModuleGraph (fontSans, fontMono) ss
  -- console window
  showConsole chanQEv

  -- render call
  render

  -- empty background with fill color
  drawBackground window
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
  _fontDefault <- imFontAtlas_AddFontDefault fonts
  fontSans <-
    withCString free_sans_path $ \cstr ->
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
  let ssref = servSess._ssServerStateRef
      chanQEv = cliSess._csPublisherEvent

  -- main loop
  whileM $ do
    ss <- readTVarIO ssref
    singleFrame io assets window ss chanQEv
    -- loop is going on while the value from the following statement is True.
    not . toBool <$> glfwWindowShouldClose window

  -- close window
  finalize ctxt window
