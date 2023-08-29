{-# LANGUAGE OverloadedRecordDot #-}

module Handler
  ( -- * low-level
    sendToControl,

    -- * high-level handler
    handleMove,
    handleClick,
    handleScroll,
  )
where

import Control.Concurrent.STM
  ( atomically,
    writeTQueue,
  )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Bits ((.|.))
import Data.Text (Text)
import Foreign.Marshal.Utils (toBool)
import GHCSpecter.UI.Types.Event
  ( Event (..),
    MouseEvent (..),
    ScrollDirection (..),
    UserEvent (..),
  )
import ImGui qualified
import ImGui.Enum (ImGuiInputFlags_ (..), ImGuiKey (..))
import Util.Render
  ( ImRender (..),
    ImRenderState (..),
    SharedState (..),
  )

sendToControl :: SharedState UserEvent -> UserEvent -> IO ()
sendToControl shared uev =
  atomically $ writeTQueue (shared.sharedChanQEv) (UsrEv uev)

handleMove :: Text -> ImRender UserEvent ()
handleMove scene_id = do
  renderState <- ImRender ask
  let shared = renderState.currSharedState
  when (shared.sharedIsMouseMoved) $ do
    case shared.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
            xy = (x' - ox, y' - oy)

        isHovered <- toBool <$> liftIO (ImGui.isItemHovered 0)
        when isHovered $
          liftIO $
            sendToControl shared (MouseEv (MouseMove (Just scene_id) xy))

handleClick :: Text -> ImRender UserEvent ()
handleClick scene_id = do
  renderState <- ImRender ask
  let shared = renderState.currSharedState
  when (shared.sharedIsClicked) $ do
    case shared.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
            xy = (x' - ox, y' - oy)

        isHovered <- toBool <$> liftIO (ImGui.isItemHovered 0)
        when isHovered $
          liftIO $
            sendToControl shared (MouseEv (MouseClick (Just scene_id) xy))

handleScroll :: Text -> ImRender UserEvent ()
handleScroll scene_id = do
  let key_wheel =
        fromIntegral $
          fromEnum ImGuiKey_MouseWheelX
            .|. fromEnum ImGuiKey_MouseWheelY
      -- key_ctrl =
      --  fromIntegral $
      --    fromEnum ImGuiMod_Ctrl
      flags =
        fromIntegral $
          fromEnum ImGuiInputFlags_CondDefault_
  liftIO $ ImGui.setItemKeyOwner key_wheel flags

  renderState <- ImRender ask
  let shared = renderState.currSharedState
      (ox, oy) = renderState.currOrigin
      (wheelX, wheelY) = shared.sharedMouseWheel
      eps = 1e-3
  -- isCtrlDown = shared.sharedCtrlDown
  -- liftIO $ putStrLn "handleScroll1"
  case shared.sharedMousePos of
    Nothing -> pure ()
    Just (x, y) ->
      when (wheelX > eps || wheelX < -eps || wheelY > eps || wheelY < -eps) $ do
        let x' = fromIntegral x
            y' = fromIntegral y
            xy = (x' - ox, y' - oy)
            dx = wheelX * 5.0
            dy = wheelY * 5.0
        liftIO $ do
          -- putStrLn "handleScroll2"
          sendToControl
            shared
            (MouseEv (Scroll ScrollDirectionLeft xy (dx, dy)))
