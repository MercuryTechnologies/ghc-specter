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
import Data.Traversable (for)
import FFICXX.Runtime.Cast (FPtr (cast_fptr_to_obj))
import Foreign.C.Types (CFloat, CInt, CUInt)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Graphics.DSL
  ( DrawText (..),
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Shape (..),
  )
import GHCSpecter.Layouter.Text (MonadTextLayout (..))
import ImGui
import STD.Deletable (delete)
import StorableInstances ()

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
renderPrimitive (Primitive (SPolyline (Polyline xy0 xys xy1 color swidth)) _ _) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        (x0, y0) = xy0
        (x1, y1) = xy1
        nPoints = length xys + 2
    -- TODO: make a utility function for this tedious and error-prone process
    allocaArray nPoints $ \(pp :: Ptr ImVec2) -> do
      p0 <- newImVec2 (realToFrac x0 + ox) (realToFrac y0 + oy)
      pokeElemOff pp 0 p0
      delete p0
      p1 <- newImVec2 (realToFrac x1 + ox) (realToFrac y1 + oy)
      pokeElemOff pp (nPoints - 1) p1
      delete p1
      for (zip [1 ..] xys) $ \(i, (x, y)) -> do
        p <- newImVec2 (realToFrac x + ox) (realToFrac y + oy)
        pokeElemOff pp i p
        delete p
      let p :: ImVec2 = cast_fptr_to_obj (castPtr pp)
      imDrawList_AddPolyline
        s.currDrawList
        p
        (fromIntegral nPoints)
        s.currColor
        0
        s.currThickness
renderPrimitive (Primitive (SDrawText (DrawText (x, y) _pos _font color _fontSize msg)) _ _) = pure ()
