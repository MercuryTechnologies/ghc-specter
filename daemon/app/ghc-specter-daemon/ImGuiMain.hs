{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGuiMain (uiMain) where

import Control.Concurrent.STM
  ( atomically,
    readTVarIO,
    writeTQueue,
  )
import Control.Monad.Extra (whenM, whileM)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString)
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
import GeneralUtil
  ( c_toImU32,
    finalize,
    initialize,
    showFramerate,
  )
import ImGui
import ImGui.Enum (ImGuiWindowFlags_ (..))
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import RenderUtil
  ( ImRenderState (..),
    renderPrimitive,
    runImRender,
  )
import STD.Deletable (delete)

showModuleGraph :: ServerState -> IO ()
showModuleGraph ss = do
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
      colf <- newImVec4 0.1 0.1 0.4 1.0
      col_ <- newImColor colf
      col <- c_toImU32 col_
      delete colf
      let rnd = 0.0
          flag = 0
          th = 3.0
      p <- getCursorScreenPos
      px <- imVec2_x_get p
      py <- imVec2_y_get p
      let renderState =
            ImRenderState
              { currDrawList = draw_list,
                currOrigin = (px, py),
                currColor = col,
                currRounding = rnd,
                currFlag = flag,
                currThickness = th
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

singleFrame ::
  ImGuiIO ->
  GLFWwindow ->
  -- TODO: for now. server state should be taken from csPublisherState
  ServerSession ->
  ClientSession ->
  IO Bool
singleFrame io window servSess cliSess = do
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  -- main drawing part
  showFramerate io
  -- TODO: temporarily
  let ssref = servSess._ssServerStateRef
  ss <- readTVarIO ssref
  showModuleGraph ss
  _ <- begin ("ghc-specter console" :: CString) nullPtr 0
  -- Buttons return true when clicked (most widgets return true when edited/activated)
  whenM (toBool <$> button (":focus 1" :: CString)) $ do
    let chanQEv = cliSess._csPublisherEvent
    atomically $
      writeTQueue
        chanQEv
        (UsrEv (ConsoleEv (ConsoleTab (DriverId 1))))
  whenM (toBool <$> button (":next" :: CString)) $ do
    let chanQEv = cliSess._csPublisherEvent
    atomically $
      writeTQueue
        chanQEv
        (UsrEv (ConsoleEv (ConsoleButtonPressed True ":next")))
  end

  render

  -- c_draw_shim window clear_color
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
  imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
  glfwSwapBuffers window

  not . toBool <$> glfwWindowShouldClose window

uiMain :: ServerSession -> ClientSession -> IO ()
uiMain servSess cliSess = do
  (ctxt, io, window) <- initialize

  -- main loop
  whileM $
    singleFrame io window servSess cliSess

  finalize ctxt window
