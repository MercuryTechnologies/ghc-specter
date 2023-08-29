{-# LANGUAGE OverloadedRecordDot #-}

module Handler
  ( -- * low-level
    sendToControl,

    -- * high-level handler
    handleMove,
    handleClick,
    handleScrollOrZoom,
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
            sendToControl shared (MouseEv (MouseMove scene_id xy))

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
            sendToControl shared (MouseEv (MouseClick scene_id xy))

handleScrollOrZoom :: Text -> ImRender UserEvent ()
handleScrollOrZoom scene_id = do
  let key_wheel =
        fromIntegral $
          fromEnum ImGuiKey_MouseWheelX
            .|. fromEnum ImGuiKey_MouseWheelY
      flags =
        fromIntegral $
          fromEnum ImGuiInputFlags_CondDefault_
  liftIO $ ImGui.setItemKeyOwner key_wheel flags

  renderState <- ImRender ask
  let shared = renderState.currSharedState
      (ox, oy) = renderState.currOrigin
      (wheelX, wheelY) = shared.sharedMouseWheel
      eps = 1e-3
      isCtrlDown = shared.sharedCtrlDown
  case shared.sharedMousePos of
    Nothing -> pure ()
    Just (x, y) ->
      when (wheelX > eps || wheelX < -eps || wheelY > eps || wheelY < -eps) $ do
        let x' = fromIntegral x
            y' = fromIntegral y
            xy = (x' - ox, y' - oy)
        if isCtrlDown
          then do
            let s_ = 1.0 - wheelY * 0.1
                s
                  | s_ > 1.1 = 1.1
                  | s_ < 0.9 = 0.9
                  | otherwise = s_
            liftIO $ do
              sendToControl
                shared
                (MouseEv (ZoomUpdate scene_id xy s))
          else do
            let dx = wheelX * 5.0
                dy = wheelY * 5.0
            liftIO $ do
              sendToControl
                shared
                (MouseEv (Scroll scene_id xy (dx, dy)))
