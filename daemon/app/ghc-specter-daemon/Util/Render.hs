{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util.Render
  ( SharedState (..),
    ImRenderState (..),
    ImRender (..),
    runImRender,
    renderPrimitive,
  )
where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteString (useAsCString)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import FFICXX.Runtime.Cast (FPtr (cast_fptr_to_obj))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Graphics.DSL
  ( DrawText (..),
    EventMap,
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Shape (..),
    TextFontFace (..),
    TextPosition (LowerLeft, UpperLeft),
  )
import GHCSpecter.UI.Types.Event (Event)
import ImGui
import STD.Deletable (delete)
import Util.Color (getNamedColor)
import Util.Orphans ()

data SharedState = SharedState
  { sharedMousePos :: Maybe (Int, Int),
    sharedIsMouseMoved :: Bool,
    sharedChanQEv :: TQueue Event,
    sharedFontSans :: ImFont,
    sharedFontMono :: ImFont
  }

data ImRenderState e = ImRenderState
  { currSharedState :: SharedState,
    currDrawList :: ImDrawList,
    currOrigin :: (Double, Double),
    currEventMap :: TVar [EventMap e]
  }

newtype ImRender e a = ImRender
  { unImRender :: ReaderT (ImRenderState e) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

runImRender :: ImRenderState e -> ImRender e a -> IO a
runImRender s action = runReaderT (unImRender action) s

--
--
--

renderPrimitive ::
  Primitive e ->
  ImRender e ()
renderPrimitive (Primitive (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) _ _mhitEvent) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        x' = realToFrac (ox + x)
        y' = realToFrac (oy + y)
        w' = realToFrac w
        h' = realToFrac h
    v1 <- newImVec2 x' y'
    v2 <- newImVec2 (x' + w') (y' + h')
    for_ mbkg $ \bkg -> do
      col <- getNamedColor bkg
      imDrawList_AddRectFilled
        s.currDrawList
        v1
        v2
        col
        0.0
        0 -- no flag
    for_ ((,) <$> mline <*> mlwidth) $ \(line, lwidth) -> do
      col <- getNamedColor line
      imDrawList_AddRect
        s.currDrawList
        v1
        v2
        col
        0.0
        0 -- no flag
        (realToFrac lwidth)
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
      p0 <- newImVec2 (realToFrac (x0 + ox)) (realToFrac (y0 + oy))
      pokeElemOff pp 0 p0
      delete p0
      p1 <- newImVec2 (realToFrac (x1 + ox)) (realToFrac (y1 + oy))
      pokeElemOff pp (nPoints - 1) p1
      delete p1
      for_ (zip [1 ..] xys) $ \(i, (x, y)) -> do
        p <- newImVec2 (realToFrac (x + ox)) (realToFrac (y + oy))
        pokeElemOff pp i p
        delete p
      let p :: ImVec2 = cast_fptr_to_obj (castPtr pp)
      col <- getNamedColor color
      imDrawList_AddPolyline
        s.currDrawList
        p
        (fromIntegral nPoints)
        col
        0
        (realToFrac swidth)
renderPrimitive (Primitive (SDrawText (DrawText (x, y) pos font color fontSize msg)) _ _) = ImRender $ do
  s <- ask
  liftIO $ do
    case font of
      Sans -> pushFont (s.currSharedState.sharedFontSans)
      Mono -> pushFont (s.currSharedState.sharedFontMono)
    let (ox, oy) = s.currOrigin
        offsetY = case pos of
          UpperLeft -> 0
          LowerLeft -> -fontSize
        x' = realToFrac (x + ox)
        y' = realToFrac (y + oy + fromIntegral offsetY)
    v' <- newImVec2 x' y'
    col <- getNamedColor color
    useAsCString (encodeUtf8 msg) $ \cstr ->
      imDrawList_AddText
        s.currDrawList
        v'
        col
        cstr
    delete v'
    popFont
