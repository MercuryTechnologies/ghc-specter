{-# LANGUAGE OverloadedStrings #-}

module Util.GUI
  ( -- * init/close
    initialize,
    finalize,

    -- * common flags
    windowFlagsNone,
    windowFlagsNoScrollbar,
    windowFlagsScroll,
    windowFlagsNoScroll,
    defTableFlags,

    -- * general util
    showFramerate,
    paintWindow,
    globalCursorPosition,
    getOriginInImGui,
    makeTabContents,
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.|.))
import Data.Foldable (fold)
import Data.Monoid (First (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import FFICXX.Runtime.Cast (FPtr (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get,
  )
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
-- import ImPlot qualified
import STD.Deletable (delete)
import Text.Printf (printf)
import Util.Orphans ()

initialize :: String -> IO (ImGuiContext, ImGuiIO, GLFWwindow)
initialize title = do
  let glsl_version :: CString
      glsl_version = "#version 150"
  putStrLn "init1"
  successCode <- glfwInit
  if successCode /= 0
    then putStrLn "GLFW is initialized"
    else putStrLn "GLFW is not initialized"
  glfwWindowHint (0x22002 {- GLFW_CONTEXT_VERSION_MAJOR -}) 3
  putStrLn "init3"
  glfwWindowHint (0x22003 {- GLFW_CONTEXT_VERSION_MINOR -}) 2
  putStrLn "init4"
  -- 3.2+ only
  glfwWindowHint (0x22008 {- GLFW_OPENGL_PROFILE -}) (0x32001 {- GLFW_OPENGL_CORE_PROFILE -})
  putStrLn "init5"
  -- Required on Mac
  glfwWindowHint (0x22006 {- GLFW_OPENGL_FORWARD_COMPAT -}) (1 {- GL_TRUE -})
  putStrLn "init6"
  window :: GLFWwindow <-
    glfwCreateWindow
      1280
      720
      (fromString title :: CString)
      (cast_fptr_to_obj nullPtr :: GLFWmonitor)
      (cast_fptr_to_obj nullPtr :: GLFWwindow)
  if get_fptr window == nullPtr
    then putStrLn "Cannot create window"
    else putStrLn "Successful window creation"
  putStrLn $ "init7: " ++ show window
  glfwMakeContextCurrent window
  putStrLn "init8"
  -- Enable vsync
  glfwSwapInterval 1
  putStrLn "init9"
  ctxt <- createContext
  putStrLn "init10"
  -- ImPlot.createImPlotContext

  -- Setup Dear ImGui style
  -- styleColorsDark
  styleColorsLight
  putStrLn "init11"

  -- Setup Platform/Renderer backends
  b <- imGui_ImplGlfw_InitForOpenGL window (fromBool True)
  putStrLn ("after InitForOpenGL: " <> show b)
  putStrLn "init12"
  _ <- imGui_ImplOpenGL3_Init glsl_version
  putStrLn "init13"

  -- Enable Keyboard Controls and Gamepad Controls
  io <- getIO
  putStrLn "init14"
  flags <- imGuiIO_ConfigFlags_get io
  putStrLn "init15"
  let flags' =
        flags
          .|. fromIntegral (fromEnum ImGuiConfigFlags_NavEnableKeyboard)
          .|. fromIntegral (fromEnum ImGuiConfigFlags_NavEnableGamepad)
  imGuiIO_ConfigFlags_set io flags'

  pure (ctxt, io, window)

finalize :: ImGuiContext -> GLFWwindow -> IO ()
finalize ctxt window = do
  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt

  glfwDestroyWindow window
  glfwTerminate

--
--
--

windowFlagsNone :: CInt
windowFlagsNone =
  fromIntegral $
    fromEnum ImGuiWindowFlags_None

windowFlagsNoScrollbar :: CInt
windowFlagsNoScrollbar =
  fromIntegral $
    fromEnum ImGuiWindowFlags_NoScrollbar

windowFlagsScroll :: CInt
windowFlagsScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_AlwaysVerticalScrollbar
      .|. fromEnum ImGuiWindowFlags_AlwaysHorizontalScrollbar

windowFlagsNoScroll :: CInt
windowFlagsNoScroll =
  fromIntegral $
    fromEnum ImGuiWindowFlags_NoScrollbar
      .|. fromEnum ImGuiWindowFlags_NoScrollWithMouse

defTableFlags :: CInt
defTableFlags =
  fromIntegral $
    fromEnum ImGuiTableFlags_BordersOuter
      .|. fromEnum ImGuiTableFlags_BordersV
      .|. fromEnum ImGuiTableFlags_RowBg
      .|. fromEnum ImGuiTableFlags_Resizable
      .|. fromEnum ImGuiTableFlags_Reorderable

showFramerate :: ImGuiIO -> IO String
showFramerate io = do
  framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
  pure $
    printf "%.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate

paintWindow :: GLFWwindow -> (Double, Double, Double) -> IO ()
paintWindow window (r, g, b) =
  alloca $ \p_dispW ->
    alloca $ \p_dispH -> do
      glfwGetFramebufferSize window p_dispW p_dispH
      dispW <- peek p_dispW
      dispH <- peek p_dispH
      glViewport 0 0 dispW dispH
      glClearColor (realToFrac r) (realToFrac g) (realToFrac b) 1.0
      glClear 0x4000 {- GL_COLOR_BUFFER_BIT -}

globalCursorPosition :: IO (Maybe (Int, Int))
globalCursorPosition = do
  -- mouse event handling
  mouse_pos <- getMousePos
  -- mouse position is regarded as integer to reduce noise.
  mouse_x <- floor @_ @Int <$> imVec2_x_get mouse_pos
  mouse_y <- floor @_ @Int <$> imVec2_y_get mouse_pos
  delete mouse_pos
  -- TODO: do this correctly
  if mouse_x > -10_000 && mouse_x < 10_000 && mouse_y > -10_000 && mouse_y < 10_000
    then pure (Just (mouse_x, mouse_y))
    else pure Nothing

getOriginInImGui :: IO (Double, Double)
getOriginInImGui = do
  p <- getCursorScreenPos
  px <- imVec2_x_get p
  py <- imVec2_y_get p
  pure (realToFrac px, realToFrac py)

makeTabContents :: (MonadIO m, Eq tag) => (Maybe tag) -> [(tag, Text, m ())] -> m (Maybe tag)
makeTabContents mpreselected = fmap (getFirst . fold) . traverse go . zip [1 ..]
  where
    go (i, (tag, title, mkItem)) = do
      let title' = fromString (T.unpack title) :: CString
      isSelected <-
        if Just tag == mpreselected
          then
            let flagSelected = fromIntegral (fromEnum ImGuiTabItemFlags_SetSelected)
             in toBool <$> liftIO (beginTabItem title' nullPtr flagSelected)
          else toBool <$> liftIO (beginTabItem_ title')
      when isSelected $ do
        liftIO $ pushID i
        zerovec <- liftIO $ newImVec2 0 0
        _ <- liftIO $ beginChild ("#tab" :: CString) zerovec (fromBool False) windowFlagsNoScroll
        void mkItem
        liftIO endChild
        liftIO $ delete zerovec
        liftIO $ popID
        liftIO endTabItem
      if isSelected
        then pure (First (Just tag))
        else pure (First Nothing)
