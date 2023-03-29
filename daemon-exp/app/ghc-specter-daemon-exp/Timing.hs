{-# LANGUAGE OverloadedStrings #-}

module Timing (
  renderTiming,
) where

import GI.Cairo.Render qualified as R
import Types (ViewBackend)
import Util (drawText)

renderTiming :: ViewBackend -> R.Render ()
renderTiming vb = do
  R.setSourceRGBA 0 0 0 1
  drawText vb 36 (100, 100) "No timing implementation yet"
