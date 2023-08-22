{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.GUI
  ( -- * init/close
    initialize,
    finalize,
    showFramerate,
  )
where

import Data.Bits ((.|.))
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (FPtr (..))
import Foreign.C.String (CString, newCString, withCString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (nullPtr)
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get,
  )
import ImPlot qualified
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import Util.Orphans ()

initialize :: IO (ImGuiContext, ImGuiIO, GLFWwindow)
initialize = do
  let glsl_version :: CString
      glsl_version = "#version 150"
  glfwInit
  glfwWindowHint (0x22002 {- GLFW_CONTEXT_VERSION_MAJOR -}) 3
  glfwWindowHint (0x22003 {- GLFW_CONTEXT_VERSION_MINOR -}) 2
  -- 3.2+ only
  glfwWindowHint (0x22008 {- GLFW_OPENGL_PROFILE -}) (0x32001 {- GLFW_OPENGL_CORE_PROFILE -})
  -- Required on Mac
  glfwWindowHint (0x22006 {- GLFW_OPENGL_FORWARD_COMPAT -}) (1 {- GL_TRUE -})
  window :: GLFWwindow <-
    glfwCreateWindow
      1280
      720
      ("ImPlot Haskell Demo" :: CString)
      (cast_fptr_to_obj nullPtr :: GLFWmonitor)
      (cast_fptr_to_obj nullPtr :: GLFWwindow)
  glfwMakeContextCurrent window
  -- Enable vsync
  glfwSwapInterval 1
  ctxt <- createContext
  ImPlot.createImPlotContext

  -- Setup Dear ImGui style
  -- styleColorsDark
  styleColorsLight

  -- Setup Platform/Renderer backends
  _ <- imGui_ImplGlfw_InitForOpenGL window (fromBool True)
  _ <- imGui_ImplOpenGL3_Init glsl_version

  -- Enable Keyboard Controls and Gamepad Controls
  io <- getIO
  flags <- imGuiIO_ConfigFlags_get io
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

showFramerate :: ImGuiIO -> IO ()
showFramerate io = do
  _ <- begin ("Framerate monitor" :: CString) nullPtr 0
  framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
  withCString (printf "Application average %.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate) $ \c_str ->
    textUnformatted c_str
  end