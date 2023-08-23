{-# LANGUAGE OverloadedRecordDot #-}

module Handler
  ( handleMove,
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

handleMove :: (Double, Double) -> ImRender UserEvent ()
handleMove (totalW, totalH) = do
  renderState <- ImRender ask
  when (renderState.currSharedState.sharedIsMouseMoved) $ do
    case renderState.currSharedState.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
        when (x' >= ox && x' <= ox + totalW && y' >= oy && y' <= oy + totalH) $
          liftIO $ do
            let xy = (x' - ox, y' - oy)
            atomically $
              writeTQueue
                (renderState.currSharedState.sharedChanQEv)
                (UsrEv $ MouseEv $ MouseMove xy)

handleClick :: (Double, Double) -> ImRender UserEvent ()
handleClick (totalW, totalH) = do
  renderState <- ImRender ask
  when (renderState.currSharedState.sharedIsClicked) $ do
    case renderState.currSharedState.sharedMousePos of
      Nothing -> pure ()
      Just (x, y) -> do
        let x' = fromIntegral x
            y' = fromIntegral y
            (ox, oy) = renderState.currOrigin
        when (x' >= ox && x' <= ox + totalW && y' >= oy && y' <= oy + totalH) $
          liftIO $ do
            let xy = (x' - ox, y' - oy)
            atomically $
              writeTQueue
                (renderState.currSharedState.sharedChanQEv)
                (UsrEv $ MouseEv $ MouseClick xy)
