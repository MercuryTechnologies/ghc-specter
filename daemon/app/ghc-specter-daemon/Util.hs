{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util
  ( finalize,
    initialize,
    makeSparkline,
    showFramerate,
  )
where

import Control.Monad.Extra (whenM)
import Data.Bits ((.|.))
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (FPtr (..))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CDouble (..), CFloat, CInt (..))
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, nullPtr)
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get,
  )
import ImPlot qualified
import ImPlot.Enum
import ImPlot.TH qualified as TH
import ImPlot.Template
import STD.Deletable (delete)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CFloat|],
    TPInfo
      { tpinfoCxxType = "float",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "float"
      }
  )

-- this is a hack. but it's the best up to now.
TH.genPlotLine1InstanceFor
  CPrim
  ( [t|Ptr CFloat|],
    TPInfo
      { tpinfoCxxType = "float",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "float"
      }
  )

showFramerate :: ImGuiIO -> IO ()
showFramerate io = do
  _ <- begin ("Framerate monitor" :: CString) nullPtr
  framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
  withCString (printf "Application average %.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate) $ \c_str ->
    textUnformatted c_str
  end

makeSparkline :: Ptr CFloat -> Int -> IO ()
makeSparkline pdat offset = do
  spark_size <- newImVec2 (-1) 35
  zero <- newImVec2 0 0
  ImPlot.pushStyleVar (fromIntegral (fromEnum ImPlotStyleVar_PlotPadding)) zero
  let spark_flags =
        fromIntegral $
          fromEnum ImPlotFlags_CanvasOnly
            .|. fromEnum ImPlotFlags_NoChild
      no_deco = fromIntegral (fromEnum ImPlotAxisFlags_NoDecorations)
  whenM (toBool <$> ImPlot.beginPlot ("1" :: CString) spark_size spark_flags) $ do
    ImPlot.setupAxes (nullPtr :: CString) (nullPtr :: CString) no_deco no_deco
    plotLine1 "##spark" pdat 100 1 0 0 (fromIntegral offset)
    ImPlot.endPlot
  delete spark_size
  delete zero

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
