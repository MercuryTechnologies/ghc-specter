{-# LANGUAGE ForeignFunctionInterface #-}
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
import Control.Monad.Extra (ifM, loopM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Maybe (isNothing)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Marshal.Utils (toBool)
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
  ( ImGuiDir_ (..),
    ImGuiKey (..),
    ImGuiMouseButton_ (..),
  )
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_DisplayFramebufferScale_get,
    imGuiIO_FontGlobalScale_set,
    imGuiIO_Fonts_get,
    imGuiIO_MouseWheelH_get,
    imGuiIO_MouseWheel_get,
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
    windowFlagsScroll,
  )
import Util.Render (SharedState (..))

foreign import ccall unsafe "scale"
  c_scale :: IO CInt

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

  -- initialize event map for this frame
  let emref = oldShared.sharedEventMap
  atomically $ writeTVar emref []
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
  let upd1
        | oldShared.sharedMousePos == mxy || isNothing mxy = \s -> s {sharedIsMouseMoved = False}
        | otherwise = \s -> s {sharedMousePos = mxy, sharedIsMouseMoved = True}
      upd2
        | isClicked = \s -> s {sharedIsClicked = True}
        | otherwise = \s -> s {sharedIsClicked = False}
      upd3 = \s -> s {sharedMouseWheel = (wheelX, wheelY), sharedCtrlDown = isCtrlDown}
      newShared = upd3 . upd2 . upd1 $ oldShared

  newShared' <- flip runReaderT newShared $ do
    -- main window
    _ <- liftIO $ begin ("main" :: CString) nullPtr 0
    let mnextTab = ui._uiModel._modelTabDestination
    tabState <-
      ifM
        (toBool <$> liftIO (beginTabBar ("#main-tabbar" :: CString)))
        ( do
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
            when (newShared.sharedTabState /= tabState) $
              case tabState of
                Nothing -> pure ()
                Just tab -> liftIO $ sendToControl newShared (TabEv tab)
            pure tabState
        )
        (pure (newShared.sharedTabState))
    liftIO end

    -- module-in-progress window
    _ <- liftIO $ begin ("Compilation Status" :: CString) nullPtr windowFlagsScroll
    Session.renderCompilationStatus ss
    liftIO end

    -- console window
    _ <- liftIO $ begin ("console" :: CString) nullPtr windowFlagsNone
    Console.render ui ss
    liftIO end

    pure $ newShared {sharedTabState = tabState}
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

prepareAssets :: ImGuiIO -> IO (ImFont, ImFont)
prepareAssets io = do
  dir <- getDataDir
  let free_sans_path = dir </> "assets" </> "FreeSans.ttf"
      free_mono_path = dir </> "assets" </> "FreeMono.ttf"
  fonts <- imGuiIO_Fonts_get io
  -- _fontDefault <- imFontAtlas_AddFontDefault fonts
  let scale = 2.0
  _fontDefault <-
    withCString free_sans_path $ \cstr -> do
      imFontAtlas_AddFontFromFileTTF fonts cstr (13 * scale)
  v <- imGuiIO_DisplayFramebufferScale_get io
  scaleX <- imVec2_x_get v
  scaleY <- imVec2_y_get v
  print (scaleX, scaleY)
  imGuiIO_FontGlobalScale_set io (1.0 / scale)
  fontSans <-
    withCString free_sans_path $ \cstr -> do
      imFontAtlas_AddFontFromFileTTF fonts cstr (8 * scale)
  fontMono <-
    withCString free_mono_path $ \cstr ->
      imFontAtlas_AddFontFromFileTTF fonts cstr (8 * scale)
  s <- c_scale
  putStrLn $ "scale = " <> show s
  pure (fontSans, fontMono)

main ::
  ServerSession ->
  ClientSession ->
  (TVar [EventMap UserEvent], TVar Stage, TVar Bool) ->
  IO ()
main servSess cliSess (em_ref, stage_ref, console_scroll_ref) = do
  -- initialize window
  (ctxt, io, window) <- initialize "ghc-specter"
  -- prepare assets (fonts)
  (fontSans, fontMono) <- prepareAssets io

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
            sharedFontSans = fontSans,
            sharedFontMono = fontMono,
            sharedEventMap = em_ref,
            sharedStage = stage_ref,
            sharedConsoleInput = p_consoleInput,
            sharedWillScrollDownConsole = console_scroll_ref
          }

  -- main loop
  flip loopM shared0 $ \oldShared -> do
    ui <- readTVarIO uiref
    ss <- readTVarIO ssref
    -- TODO: this is ugly. should be handled in a more disciplined way.
    newShared <- singleFrame io window ui ss oldShared
    -- loop is going on while the value from the following statement is True.
    willClose <- toBool <$> glfwWindowShouldClose window
    if willClose then pure (Right ()) else pure (Left newShared)

  free p_consoleInput
  -- close window
  finalize ctxt window
