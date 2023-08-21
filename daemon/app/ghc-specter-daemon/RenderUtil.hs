{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module RenderUtil
  ( ImRenderState (..),
    ImRender (..),
    runImRender,
    renderPrimitive,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Foreign.C.Types (CFloat, CInt, CUInt)
import GHCSpecter.Graphics.DSL
  ( DrawText (..),
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Shape (..),
  )
import GHCSpecter.Layouter.Text (MonadTextLayout (..))
import ImGui
import STD.Deletable

data ImRenderState = ImRenderState
  { currDrawList :: ImDrawList,
    currOrigin :: (CFloat, CFloat),
    currColor :: CUInt,
    currRounding :: CFloat,
    currFlag :: CInt,
    currThickness :: CFloat
  }

newtype ImRender a = ImRender
  { unImRender :: ReaderT ImRenderState IO a
  }
  deriving (Functor, Applicative, Monad)

runImRender :: ImRenderState -> ImRender a -> IO a
runImRender s action = runReaderT (unImRender action) s

renderPrimitive ::
  Primitive e ->
  ImRender ()
renderPrimitive (Primitive (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) _ _mhitEvent) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        x' = ox + realToFrac x
        y' = oy + realToFrac y
        w' = realToFrac w
        h' = realToFrac h
    v1 <- newImVec2 x' y'
    v2 <- newImVec2 (x' + w') (y' + h')
    imDrawList_AddRect
      s.currDrawList
      v1
      v2
      s.currColor
      s.currRounding
      s.currFlag
      s.currThickness
    delete v1
    delete v2
renderPrimitive (Primitive (SPolyline (Polyline start xys end color swidth)) _ _) = pure ()
renderPrimitive (Primitive (SDrawText (DrawText (x, y) _pos _font color _fontSize msg)) _ _) = pure ()
