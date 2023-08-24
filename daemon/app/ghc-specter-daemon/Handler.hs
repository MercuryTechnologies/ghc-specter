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
import GHCSpecter.UI.Types.Event
  ( Event (..),
    MouseEvent (..),
    UserEvent (..),
  )
import Util.Render
  ( ImRender (..),
    ImRenderState (..),
    SharedState (..),
  )

sendToControl :: SharedState UserEvent -> UserEvent -> IO ()
sendToControl shared uev =
  atomically $ writeTQueue (shared.sharedChanQEv) (UsrEv uev)

handleMove :: (Double, Double) -> ImRender UserEvent ()
handleMove (totalW, totalH) = do
  renderState <- ImRender ask
  let shared = renderState.currSharedState
  when (shared.sharedIsMouseMoved) $ do
    case shared.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
        when (x' >= ox && x' <= ox + totalW && y' >= oy && y' <= oy + totalH) $ do
          let xy = (x' - ox, y' - oy)
          liftIO $ sendToControl shared (MouseEv (MouseMove xy))

handleClick :: (Double, Double) -> ImRender UserEvent ()
handleClick (totalW, totalH) = do
  renderState <- ImRender ask
  let shared = renderState.currSharedState
  when (shared.sharedIsClicked) $ do
    case shared.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
        when (x' >= ox && x' <= ox + totalW && y' >= oy && y' <= oy + totalH) $ do
          let xy = (x' - ox, y' - oy)
          liftIO $ sendToControl shared (MouseEv (MouseClick xy))
