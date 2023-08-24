{-# LANGUAGE OverloadedRecordDot #-}

module Handler
  ( -- * low-level
    sendToControl,

    -- * high-level handler
    handleMove,
    handleClick,
  )
where

import Control.Concurrent.STM
  ( atomically,
    writeTQueue,
  )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import Foreign.Marshal.Utils (toBool)
import GHCSpecter.UI.Types.Event
  ( Event (..),
    MouseEvent (..),
    UserEvent (..),
  )
import ImGui (isItemHovered)
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

        isHovered <- toBool <$> liftIO (isItemHovered 0)
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

        isHovered <- toBool <$> liftIO (isItemHovered 0)
        when isHovered $
          liftIO $
            sendToControl shared (MouseEv (MouseClick (Just scene_id) xy))
