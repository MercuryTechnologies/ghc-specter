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
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bits ((.|.))
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
  ( Tab (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui
import ImGui.Enum
  ( ImGuiInputFlags_ (..),
    ImGuiKey (..),
    ImGuiMouseButton_ (..),
    ImGuiWindowFlags_ (..),
  )
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_Fonts_get,
    imGuiIO_MouseWheelH_get,
    imGuiIO_MouseWheel_get,
  )
import Paths_ghc_specter_daemon (getDataDir)
import Render.Console (consoleInputBufferSize)
import Render.Console qualified as Console (render)
import Render.ModuleGraph qualified as ModuleGraph
  ( render,
    renderBlockerGraph,
  )
import Render.Session qualified as Session
  ( renderCompilationStatus,
    renderSession,
  )
import Render.SourceView qualified as SourceView (render)
import Render.TimingView qualified as Timing (render, renderMemoryView)
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.GUI
  ( currentOrigin,
    finalize,
    globalCursorPosition,
    initialize,
    makeTabContents,
    paintWindow,
    showFramerate,
    windowFlagsNone,
    windowFlagsScroll,
  )
import Util.Render (SharedState (..))

tabSession :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabSession ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#session" :: CString) zerovec (fromBool False) windowFlagsScroll
  Session.renderSession ui ss
  liftIO endChild
  liftIO $ delete zerovec

-- tabModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
-- tabModuleGraph = ModuleGraph.render

tabSourceView :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabSourceView ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#source-view" :: CString) zerovec (fromBool False) windowFlagsScroll
  SourceView.render ui ss
  liftIO endChild
  liftIO $ delete zerovec

tabTiming :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabTiming ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  let flags =
        fromIntegral $
          fromEnum ImGuiWindowFlags_NoScrollbar
            .|. fromEnum ImGuiWindowFlags_NoScrollWithMouse
  _ <- liftIO $ beginChild ("#timing-view" :: CString) zerovec (fromBool False) flags
  Timing.render ui ss
  liftIO endChild
  liftIO $ delete zerovec

tabBlockerGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabBlockerGraph ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#blocker-graph" :: CString) zerovec (fromBool False) windowFlagsScroll
  ModuleGraph.renderBlockerGraph ui ss
  liftIO endChild
  liftIO $ delete zerovec

tabMemory :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabMemory ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#memory" :: CString) zerovec (fromBool False) windowFlagsScroll
  Timing.renderMemoryView ui ss
  liftIO endChild
  liftIO $ delete zerovec

{-
tabTest :: ImGuiIO -> ReaderT (SharedState UserEvent) IO ()
tabTest io = do
  shared <- ask
  let mxy = shared.sharedMousePos
  liftIO $ do
    (ox, oy) <- currentOrigin
    dummy_sz <- newImVec2 500 500
    dummy dummy_sz
    let key_wheel =
          fromIntegral $
            fromEnum ImGuiKey_MouseWheelX
              .|. fromEnum ImGuiKey_MouseWheelY
        key_ctrl =
          fromIntegral $
            fromEnum ImGuiMod_Ctrl
        flags =
          fromIntegral $
            fromEnum ImGuiInputFlags_CondDefault_
    setItemKeyOwner key_wheel flags
    wheelX <- imGuiIO_MouseWheelH_get io
    wheelY <- imGuiIO_MouseWheel_get io
    print (wheelX, wheelY)
    case mxy of
      Just (x, y) ->
        putStrLn $
          "mouse position on canvas: " <> show (fromIntegral x - ox, fromIntegral y - oy)
      Nothing -> putStrLn "cannot find the mouse position"
    b <- isKeyDown key_ctrl
    putStrLn $ "Ctrl is down: " <> show b
    pure ()
-}

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
  wheelX <- realToFrac <$> imGuiIO_MouseWheelH_get io
  wheelY <- realToFrac <$> imGuiIO_MouseWheel_get io
  let key_ctrl =
        fromIntegral $
          fromEnum ImGuiMod_Ctrl
  isCtrlDown <- toBool <$> isKeyDown key_ctrl
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
                [ (TabSession, "Session", tabSession ui ss),
                  (TabModuleGraph, "Module graph", ModuleGraph.render ui ss),
                  (TabSourceView, "Source view", tabSourceView ui ss),
                  (TabTiming, "Timing view", tabTiming ui ss),
                  (TabTiming, "Blocker graph", tabBlockerGraph ui ss),
                  (TabTiming, "Memory view", tabMemory ui ss)
                  -- (TabTiming, "test", tabTest io)
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
    --
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

main ::
  ServerSession ->
  ClientSession ->
  (TVar [EventMap UserEvent], TVar Stage) ->
  IO ()
main servSess cliSess (em_ref, stage_ref) = do
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
            sharedConsoleInput = p_consoleInput
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
