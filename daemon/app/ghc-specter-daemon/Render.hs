{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (main) where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    readTVarIO,
    writeTQueue,
    writeTVar,
  )
import Control.Monad (void, when)
import Control.Monad.Extra (loopM, whenM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Bits ((.|.))
import Data.Maybe (isNothing)
import Data.String (fromString)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (nullPtr)
import GHCSpecter.Driver.Session.Types
  ( ClientSession (..),
    ServerSession (..),
  )
import GHCSpecter.Graphics.DSL (EventMap)
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.Types (UIState (..))
import GHCSpecter.UI.Types.Event
  ( Event (..),
    Tab (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui
import ImGui.Enum
  ( ImGuiMouseButton_ (..),
    ImGuiTableFlags_ (..),
    ImGuiWindowFlags_ (..),
  )
import ImGui.ImGuiIO.Implementation (imGuiIO_Fonts_get)
import Paths_ghc_specter_daemon (getDataDir)
import Render.Console (renderConsole)
import Render.ModuleGraph (renderMainModuleGraph, renderSubModuleGraph)
import Render.Session (renderSession)
import Render.TimingView (renderMemoryView, renderTimingView)
import STD.Deletable (delete)
import System.FilePath ((</>))
import Util.GUI
  ( finalize,
    globalCursorPosition,
    initialize,
    paintWindow,
    showFramerate,
  )
import Util.Render
  ( SharedState (..),
    toTab,
  )

windowFlagsScroll :: CInt
windowFlagsScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
      .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar

makeTabContents :: (MonadIO m) => [(String, m ())] -> m [Bool]
makeTabContents = traverse go
  where
    go (title, mkItem) = do
      isSelected <- toBool <$> liftIO (beginTabItem (fromString title :: CString))
      when isSelected $ do
        void mkItem
        liftIO endTabItem
      pure isSelected

tabSession :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabSession ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#session" :: CString) zerovec (fromBool False) windowFlagsScroll
  renderSession ui ss
  liftIO endChild
  liftIO $ delete zerovec

tabModuleGraph :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabModuleGraph ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  minusvec <- liftIO $ newImVec2 0 (-200)
  let flags =
        fromIntegral $
          fromEnum ImGuiTableFlags_BordersOuter
            .|. fromEnum ImGuiTableFlags_BordersV
            .|. fromEnum ImGuiTableFlags_RowBg
            .|. fromEnum ImGuiTableFlags_Resizable
            .|. fromEnum ImGuiTableFlags_Reorderable

  whenM (toBool <$> liftIO (beginTable ("##table" :: CString) 1 flags)) $ do
    liftIO $ tableSetupColumn_ ("graph" :: CString)
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    _ <- liftIO $ beginChild ("#main-modgraph" :: CString) minusvec (fromBool False) windowFlagsScroll
    renderMainModuleGraph ui ss
    liftIO endChild
    --
    liftIO $ tableNextRow 0
    liftIO $ tableSetColumnIndex 0
    _ <- liftIO $ beginChild ("#sub-modgraph" :: CString) zerovec (fromBool False) windowFlagsScroll
    renderSubModuleGraph ui ss
    liftIO endChild
    liftIO endTable
  liftIO $ delete zerovec
  liftIO $ delete minusvec

tabTiming :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabTiming ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#timing" :: CString) zerovec (fromBool False) windowFlagsScroll
  renderTimingView ui ss
  liftIO endChild
  liftIO $ delete zerovec

tabMemory :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
tabMemory ui ss = do
  zerovec <- liftIO $ newImVec2 0 0
  _ <- liftIO $ beginChild ("#memory" :: CString) zerovec (fromBool False) windowFlagsScroll
  renderMemoryView ui ss
  liftIO endChild
  liftIO $ delete zerovec

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
    -- main window
    _ <- liftIO $ begin ("main" :: CString) nullPtr 0
    _ <- liftIO $ beginTabBar ("##TabBar" :: CString)
    --
    tabState <-
      makeTabContents
        [ ("Session", tabSession ui ss),
          ("Module graph", tabModuleGraph ui ss),
          ("Timing view", tabTiming ui ss),
          ("Memory view", tabMemory ui ss)
        ]
    liftIO endTabBar
    liftIO end

    -- console window
    _ <- liftIO $ begin ("console" :: CString) nullPtr windowFlagsScroll
    renderConsole
    liftIO end

    -- post-rendering event handling: there are events discovered after rendering such as Tab.
    when (newShared.sharedTabState /= tabState) $
      case toTab tabState of
        Nothing -> pure ()
        Just tab -> liftIO $ sendToControl newShared (TabEv tab)
    pure $ newShared {sharedTabState = tabState}

  --
  -- finalize rendering by compositing render call
  --
  render
  --
  -- hit testing by global mouse coordinate is now meaningful from here.
  --
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

main :: ServerSession -> ClientSession -> TVar [EventMap UserEvent] -> IO ()
main servSess cliSess emref = do
  -- initialize window
  (ctxt, io, window) <- initialize "ghc-specter"
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
            sharedTabState = [True, False, False],
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
