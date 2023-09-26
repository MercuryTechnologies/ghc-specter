{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (main) where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State (execStateT, get, modify')
import Data.Bits ((.|.))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isNothing)
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (nullPtr)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
  )
import GHCSpecter.Graphics.DSL
  ( EventMap,
    Stage,
  )
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types
  ( UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( MouseEvent (..),
    Tab (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui
import ImGui.Enum
  ( ImGuiCond_ (..),
    ImGuiDir_ (..),
    ImGuiKey (..),
    ImGuiMouseButton_ (..),
    ImGuiStyleVar_ (..),
    ImGuiWindowFlags_ (..),
  )
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_FontGlobalScale_set,
    imGuiIO_Fonts_get,
    imGuiIO_MouseDelta_get,
    imGuiIO_MouseWheelH_get,
    imGuiIO_MouseWheel_get,
  )
import ImGui.ImGuiViewport.Implementation
  ( imGuiViewport_WorkPos_get,
    imGuiViewport_WorkSize_get,
  )
import ImGui.ImVec2.Implementation
  ( imVec2_x_get,
    imVec2_y_get,
  )
import Paths_ghc_specter_daemon (getDataDir)
import Render.BlockerView qualified as BlockerView
import Render.Console (consoleInputBufferSize)
import Render.Console qualified as Console
import Render.ModuleGraph qualified as ModuleGraph
import Render.Session qualified as Session
import Render.SourceView qualified as SourceView
import Render.TimingView qualified as Timing
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.GUI
  ( finalize,
    globalCursorPosition,
    initialize,
    makeTabContents,
    paintWindow,
    showFramerate,
    windowFlagsNoScroll,
    windowFlagsNone,
  )
import Util.Render
  ( SharedState (..),
    c_detectScaleFactor,
    loadAllFonts,
  )

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

  viewport <- getMainViewport
  _ <-
    beginViewportSideBar
      ("#test" :: CString)
      viewport
      (fromIntegral (fromEnum ImGuiDir_Down))
      30
      windowFlagsNoScroll
  let help = "Scroll with mouse wheel or touchpad. Ctrl+Scroll for zooming in/out.     Framerate = "
  frate_str <- showFramerate io

  withCString (help <> frate_str) $ \c_str ->
    textUnformatted c_str
  end

  mxy <- globalCursorPosition

  isClicked <- toBool <$> isMouseClicked_ (fromIntegral (fromEnum ImGuiMouseButton_Left))
  wheelX <- realToFrac <$> imGuiIO_MouseWheelH_get io
  wheelY <- realToFrac <$> imGuiIO_MouseWheel_get io
  let key_ctrl =
        fromIntegral $
          fromEnum ImGuiMod_Ctrl
  let isCtrlDown_old = oldShared.sharedCtrlDown
  isCtrlDown <- toBool <$> isKeyDown key_ctrl
  -- TODO: find a better method for this.
  when (isCtrlDown_old && not isCtrlDown) $
    sendToControl oldShared (MouseEv ZoomEnd)
  -- initialize event map for this frame
  let upd0 = \s -> s {sharedEventMap = []}
      upd1
        | oldShared.sharedMousePos == mxy || isNothing mxy = \s -> s {sharedIsMouseMoved = False}
        | otherwise = \s -> s {sharedMousePos = mxy, sharedIsMouseMoved = True}
      upd2
        | isClicked = \s -> s {sharedIsClicked = True}
        | otherwise = \s -> s {sharedIsClicked = False}
      upd3 = \s -> s {sharedMouseWheel = (wheelX, wheelY), sharedCtrlDown = isCtrlDown}
      newShared = upd3 . upd2 . upd1 . upd0 $ oldShared

  newShared' <- flip execStateT newShared $ do
    -- main menu
    whenM (toBool <$> liftIO beginMainMenuBar) $ do
      whenM (toBool <$> liftIO (beginMenu ("ghc-specter" :: CString) (fromBool True))) $ do
        b1 <- liftIO $ menuItem_ ("About ghc-specter" :: CString) (nullPtr :: CString) (fromBool False) (fromBool True)
        when (toBool b1) $
          modify' (\s -> s {sharedPopup1 = True})
        liftIO endMenu
      whenM (toBool <$> liftIO (beginMenu ("Help" :: CString) (fromBool True))) $ do
        b2 <- liftIO $ menuItem_ ("ghc-specter help" :: CString) (nullPtr :: CString) (fromBool False) (fromBool True)
        when (toBool b2) $
          modify' (\s -> s {sharedPopup2 = True})
        liftIO endMenu
      liftIO endMainMenuBar

    -- dialog box test
    when newShared.sharedPopup1 $ do
      liftIO $ openPopup ("About ghc-specter" :: CString) 0
      center <- liftIO $ imGuiViewport_GetCenter viewport
      rel_pos <- liftIO $ newImVec2 0.5 0.5
      liftIO $ setNextWindowPos center (fromIntegral (fromEnum ImGuiCond_Appearing)) rel_pos
      liftIO $ delete rel_pos
      let flag = fromIntegral (fromEnum ImGuiWindowFlags_AlwaysAutoResize)
      whenM (toBool <$> liftIO (beginPopupModal ("About ghc-specter" :: CString) nullPtr flag)) $ do
        liftIO $ textUnformatted ("ghc-specter 1.0.0.0" :: CString)
        whenM (toBool <$> liftIO (button ("close" :: CString))) $
          modify' (\s -> s {sharedPopup1 = False})
        liftIO endPopup

    when newShared.sharedPopup2 $ do
      liftIO $ openPopup ("HelpABC" :: CString) 0
      center <- liftIO $ imGuiViewport_GetCenter viewport
      rel_pos <- liftIO $ newImVec2 0.5 0.5
      liftIO $ setNextWindowPos center (fromIntegral (fromEnum ImGuiCond_Appearing)) rel_pos
      -- liftIO $ delete rel_pos
      let flag = fromIntegral (fromEnum ImGuiWindowFlags_AlwaysAutoResize)
      whenM (toBool <$> liftIO (beginPopupModal ("HelpABC" :: CString) nullPtr flag)) $ do
        liftIO $ textUnformatted ("I cannot help you now." :: CString)
        whenM (toBool <$> liftIO (button ("close" :: CString))) $
          modify' (\s -> s {sharedPopup2 = False})
        liftIO endPopup

    -- fullscreen window
    let flags =
          fromIntegral $
            fromEnum ImGuiWindowFlags_NoDecoration
              .|. fromEnum ImGuiWindowFlags_NoMove
              .|. fromEnum ImGuiWindowFlags_NoSavedSettings
    pos <- liftIO $ imGuiViewport_WorkPos_get viewport
    size <- liftIO $ imGuiViewport_WorkSize_get viewport
    zero <- liftIO $ newImVec2 0 0
    liftIO $ setNextWindowPos pos 0 zero
    liftIO $ setNextWindowSize size 0
    _ <- liftIO $ begin ("fullscreen" :: CString) nullPtr flags

    -- start splitter
    let (w, h) = newShared.sharedLeftPaneSize
    liftIO $ pushStyleVar2 (fromIntegral (fromEnum ImGuiStyleVar_ItemSpacing)) zero
    child1_size <- liftIO $ newImVec2 (realToFrac w) (realToFrac h)
    _ <- liftIO $ beginChild ("Compilation Status" :: CString) child1_size (fromBool True) 0
    Session.renderCompilationStatus ss
    liftIO endChild
    liftIO $ delete child1_size
    --
    liftIO sameLine_
    --
    vsplitter_size <- liftIO $ newImVec2 8.0 (realToFrac h)
    _ <- liftIO $ invisibleButton ("vsplitter" :: CString) vsplitter_size 0
    whenM (toBool <$> liftIO isItemActive) $ do
      delta_x <- liftIO (realToFrac <$> (imVec2_x_get =<< imGuiIO_MouseDelta_get io))
      modify' (\s -> s {sharedLeftPaneSize = (w + delta_x, h)})
    liftIO $ delete vsplitter_size
    --
    liftIO sameLine_
    --
    child2_size <- liftIO $ newImVec2 0 (realToFrac h)
    _ <- liftIO $ beginChild ("main" :: CString) child2_size (fromBool False) windowFlagsNone
    let mnextTab = ui._uiModel._modelTabDestination
    whenM (toBool <$> liftIO (beginTabBar ("#main-tabbar" :: CString))) $ do
      tabState <-
        makeTabContents
          mnextTab
          [ (TabSession, "Session", Session.render ui ss),
            (TabModuleGraph, "Module graph", ModuleGraph.render ui ss),
            (TabSourceView, "Source view", SourceView.render ui ss),
            (TabTiming, "Timing view", Timing.render ui ss),
            (TabBlocker, "Blocker graph", BlockerView.render ui ss)
          ]
      liftIO endTabBar
      -- tab event handling
      tabState0 <- (.sharedTabState) <$> get
      when (tabState0 /= tabState) $ do
        case tabState of
          Nothing -> pure ()
          Just tab -> liftIO $ sendToControl newShared (TabEv tab)
        modify' (\s -> s {sharedTabState = tabState})
    -- end of main
    liftIO $ endChild
    --
    hsplitter_size <- liftIO $ newImVec2 (-1.0) 8.0
    _ <- liftIO $ invisibleButton ("hsplitter" :: CString) hsplitter_size 0
    whenM (toBool <$> liftIO isItemActive) $ do
      delta_y <- liftIO (realToFrac <$> (imVec2_y_get =<< imGuiIO_MouseDelta_get io))
      modify' (\s -> s {sharedLeftPaneSize = (w, h + delta_y)})
    liftIO $ delete hsplitter_size
    --
    -- console window
    _ <- liftIO $ beginChild ("console" :: CString) zero (fromBool True) windowFlagsNone
    Console.render ui ss
    liftIO endChild
    --
    liftIO popStyleVar_
    -- end of fullscreen
    liftIO end
    liftIO $ delete zero

  -- pure newShared'
  --
  -- finalize rendering by compositing render call
  render
  -- empty background with fill color
  paintWindow window (0.45, 0.55, 0.60 {- bluish gray -})
  -- stage the frame
  imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
  -- commit the frame
  glfwSwapBuffers window
  --
  pure newShared'

prepareAssets :: ImGuiIO -> IO (NonEmpty (Int, ImFont), NonEmpty (Int, ImFont), Double)
prepareAssets io = do
  dir <- getDataDir
  let free_sans_path = dir </> "assets" </> "FreeSans.ttf"
      free_mono_path = dir </> "assets" </> "FreeMono.ttf"
  fonts <- imGuiIO_Fonts_get io
  scale_factor <- c_detectScaleFactor
  _fontDefault <-
    withCString free_sans_path $ \cstr -> do
      imFontAtlas_AddFontFromFileTTF fonts cstr (13 * scale_factor)
  imGuiIO_FontGlobalScale_set io (1.0 / scale_factor)
  fonts_sans <- loadAllFonts free_sans_path fonts scale_factor
  fonts_mono <- loadAllFonts free_mono_path fonts scale_factor
  _ <- imFontAtlas_Build fonts
  pure (fonts_sans, fonts_mono, realToFrac scale_factor)

main ::
  ServerSession ->
  ClientSession ->
  (TVar [EventMap UserEvent], TVar Stage, TVar Bool) ->
  IO ()
main servSess cliSess (em_ref, stage_ref, console_scroll_ref) = do
  -- initialize window
  (ctxt, io, window) <- initialize "ghc-specter"
  -- prepare assets (fonts)
  (fonts_sans, fonts_mono, scale_factor) <- prepareAssets io

  p_consoleInput <- callocBytes consoleInputBufferSize
  -- state and event channel
  let uiref = cliSess._csUIStateRef
      ssref = servSess._ssServerStateRef
      chanQEv = cliSess._csPublisherEvent
      shared0 =
        SharedState
          { sharedMousePos = Nothing,
            sharedMouseWheel = (0, 0),
            sharedCtrlDown = False,
            sharedIsMouseMoved = False,
            sharedIsClicked = False,
            sharedTabState = Nothing,
            sharedChanQEv = chanQEv,
            sharedFontsSans = fonts_sans,
            sharedFontsMono = fonts_mono,
            sharedFontScaleFactor = scale_factor,
            sharedEventMap = [],
            sharedStage = stage_ref,
            sharedConsoleInput = p_consoleInput,
            sharedWillScrollDownConsole = console_scroll_ref,
            sharedLeftPaneSize = (120, 500),
            sharedPopup1 = False,
            sharedPopup2 = False
          }

  -- main loop
  flip loopM shared0 $ \oldShared -> do
    ui <- readTVarIO uiref
    ss <- readTVarIO ssref
    newShared <- singleFrame io window ui ss oldShared
    -- flush frame state to control
    atomically $
      writeTVar em_ref newShared.sharedEventMap

    -- loop is going on while the value from the following statement is True.
    willClose <- toBool <$> glfwWindowShouldClose window
    if willClose then pure (Right ()) else pure (Left newShared)

  free p_consoleInput
  -- close window
  finalize ctxt window
