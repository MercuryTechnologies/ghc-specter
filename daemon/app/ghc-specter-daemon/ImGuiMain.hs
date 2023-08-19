{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGuiMain (uiMain) where

import Control.Monad.Extra (whileM)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef)
import Foreign.C.Types (CDouble (..), CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import ImGui
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import System.Random (randomRIO)
import Util
  ( demoLinePlots,
    demoTables,
    finalize,
    initialize,
    showFramerate,
  )

singleFrame ::
  ImGuiIO ->
  GLFWwindow ->
  ImVec4 ->
  IORef Int ->
  (Ptr CFloat, Ptr CFloat) ->
  (Ptr CDouble, Ptr CDouble) ->
  Ptr CFloat ->
  IO Bool
singleFrame io window clear_color ref_offset (px1, py1) (px2, py2) pdat = do
  modifyIORef' ref_offset (+ 1)
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  showFramerate io
  demoLinePlots (px1, py1) (px2, py2)
  demoTables ref_offset pdat
  render

  -- c_draw_shim window clear_color
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
  imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
  glfwSwapBuffers window

  not . toBool <$> glfwWindowShouldClose window

uiMain :: IO ()
uiMain = do
  (ctxt, io, window) <- initialize

  -- Our state
  clear_color <- newImVec4 0.45 0.55 0.60 1.00
  ref_offset <- newIORef (5 :: Int)

  allocaArray 1001 $ \(px1 :: Ptr CFloat) ->
    allocaArray 1001 $ \(py1 :: Ptr CFloat) ->
      allocaArray 20 $ \(px2 :: Ptr CDouble) ->
        allocaArray 20 $ \(py2 :: Ptr CDouble) ->
          allocaArray 100 $ \(pdat :: Ptr CFloat) -> do
            for_ [0 .. 99] $ \i -> do
              v <- randomRIO (0, 10)
              pokeElemOff pdat i v

            -- main loop
            whileM $
              singleFrame io window clear_color ref_offset (px1, py1) (px2, py2) pdat

  finalize ctxt window
