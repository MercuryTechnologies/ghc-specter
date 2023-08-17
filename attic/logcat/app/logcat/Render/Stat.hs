module Render.Stat
  ( drawStats,
  )
where

import Data.Text qualified as T
import GI.Cairo.Render qualified as R
import Render.Util
  ( black,
    canvasHeight,
    canvasWidth,
    drawText,
    fontSize,
    setColor,
    white,
  )
import Types (LogcatView)

drawStats :: LogcatView -> Int -> R.Render ()
drawStats vw nBytes = do
  let ulx = canvasWidth - w - 10
      uly = canvasHeight - h - 10
      w = 150
      h = 80
  setColor black
  R.rectangle ulx uly w h
  R.fill
  setColor white
  R.rectangle ulx uly w h
  R.stroke

  let msg = T.pack $ "Received bytes: " ++ show nBytes
  setColor white
  drawText vw fontSize (ulx + 5, uly + 5) msg
