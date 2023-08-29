{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Util.Render
  ( -- * state
    SharedState (..),
    ImRenderState (..),
    mkRenderState,

    -- * ImRender monad
    ImRender (..),
    runImRender,

    -- * rendering and event map
    renderShape,
    renderPrimitive,
    renderScene,
    buildEventMap,
    addEventMap,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString (useAsCString)
import Data.Foldable (for_, traverse_)
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (encodeUtf8)
import FFICXX.Runtime.Cast (FPtr (cast_fptr_to_obj))
import Foreign.C.String (CString)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Graphics.DSL
  ( DrawText (..),
    EventMap,
    Polyline (..),
    Primitive (..),
    Rectangle (..),
    Scene (..),
    Shape (..),
    Stage (..),
    TextFontFace (..),
    TextPosition (LowerLeft, UpperLeft),
    ViewPort (..),
    overlapsWith,
  )
import GHCSpecter.UI.Types.Event (Event, Tab (..))
import ImGui
import STD.Deletable (delete)
import Util.Color (getNamedColor)
import Util.GUI (currentOrigin)
import Util.Orphans ()

--
-- state
--

data SharedState e = SharedState
  { sharedMousePos :: Maybe (Int, Int),
    sharedMouseWheel :: (Double, Double),
    sharedCtrlDown :: Bool,
    sharedIsMouseMoved :: Bool,
    sharedIsClicked :: Bool,
    sharedTabState :: Maybe Tab,
    sharedChanQEv :: TQueue Event,
    sharedFontSans :: ImFont,
    sharedFontMono :: ImFont,
    sharedEventMap :: TVar [EventMap e],
    sharedStage :: TVar Stage,
    sharedConsoleInput :: CString
  }

data ImRenderState e = ImRenderState
  { currSharedState :: SharedState e,
    currDrawList :: ImDrawList,
    currOrigin :: (Double, Double),
    -- | (scaleX, scaleY)
    currScale :: (Double, Double)
  }

mkRenderState :: ReaderT (SharedState e) IO (ImRenderState e)
mkRenderState = do
  shared <- ask
  draw_list <- liftIO getWindowDrawList
  oxy <- liftIO currentOrigin
  pure
    ImRenderState
      { currSharedState = shared,
        currDrawList = draw_list,
        currOrigin = oxy,
        currScale = (1.0, 1.0)
      }

--
-- ImRender monad
--

newtype ImRender e a = ImRender
  { unImRender :: ReaderT (ImRenderState e) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (ImRenderState e))

runImRender :: ImRenderState e -> ImRender e a -> IO a
runImRender s action = runReaderT (unImRender action) s

--
--
--

mkImVec2 :: (Double, Double) -> IO ImVec2
mkImVec2 (x, y) = newImVec2 (realToFrac x) (realToFrac y)

--
--

renderShape :: Shape -> ImRender e ()
renderShape (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        x' = ox + x
        y' = oy + y
    v1 <- mkImVec2 (x', y') -- newImVec2 x' y'
    v2 <- mkImVec2 (x' + w, y' + h) -- newImVec2 (x' + w') (y' + h')
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
renderShape (SPolyline (Polyline xy0 xys xy1 color swidth)) = ImRender $ do
  s <- ask
  liftIO $ do
    let (ox, oy) = s.currOrigin
        (x0, y0) = xy0
        (x1, y1) = xy1
        nPoints = length xys + 2
    -- TODO: make a utility function for this tedious and error-prone process
    allocaArray nPoints $ \(pp :: Ptr ImVec2) -> do
      p0 <- mkImVec2 (x0 + ox, y0 + oy) -- newImVec2 (realToFrac (x0 + ox)) (realToFrac (y0 + oy))
      pokeElemOff pp 0 p0
      delete p0
      p1 <- mkImVec2 (x1 + ox, y1 + oy) -- newImVec2 (realToFrac (x1 + ox)) (realToFrac (y1 + oy))
      pokeElemOff pp (nPoints - 1) p1
      delete p1
      for_ (zip [1 ..] xys) $ \(i, (x, y)) -> do
        p <- mkImVec2 (x + ox, y + oy) -- newImVec2 (realToFrac (x + ox)) (realToFrac (y + oy))
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
renderShape (SDrawText (DrawText (x, y) pos font color fontSize msg)) = ImRender $ do
  s <- ask
  liftIO $ do
    case font of
      Sans -> pushFont (s.currSharedState.sharedFontSans)
      Mono -> pushFont (s.currSharedState.sharedFontMono)
    let (ox, oy) = s.currOrigin
        offsetY = case pos of
          UpperLeft -> 0
          LowerLeft -> -fontSize
        x' = x + ox
        y' = y + oy + fromIntegral offsetY
    v' <- mkImVec2 (x', y')
    col <- getNamedColor color
    useAsCString (encodeUtf8 msg) $ \cstr ->
      imDrawList_AddText
        s.currDrawList
        v'
        col
        cstr
    delete v'
    popFont

renderPrimitive :: Primitive e -> ImRender e ()
renderPrimitive (Primitive shape _ _) = renderShape shape

renderScene :: Scene (Primitive e) -> ImRender e ()
renderScene scene = do
  let ViewPort (cx0, cy0) (cx1, cy1) = sceneGlobalViewPort scene
      vp@(ViewPort (vx0, vy0) (vx1, vy1)) = sceneLocalViewPort scene
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
  -- cairo code for reference
  {-  lift $ do
    R.save
    R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
    R.clip
    R.translate cx0 cy0
    R.scale scaleX scaleY
    R.translate (-vx0) (-vy0)
  -}
  let overlapCheck p =
        vp `overlapsWith` primBoundingBox p
      filtered = filter overlapCheck (sceneElements scene)
  local
    ( \s ->
        let (ox, oy) = s.currOrigin
            (ox', oy') = (ox - vx0, oy - vy0)
         in s
              { currOrigin = (ox', oy'),
                currScale = (scaleX, scaleY)
              }
    )
    $ traverse_ renderPrimitive filtered

-- lift R.restore

buildEventMap :: Scene (Primitive e) -> EventMap e
buildEventMap scene =
  let -- TODO: handle events for other shapes
      extractEvent (Primitive (SRectangle (Rectangle (x, y) w h _ _ _)) _ (Just hitEvent)) =
        Just (hitEvent, ViewPort (x, y) (x + w, y + h))
      extractEvent _ = Nothing
      emap = scene {sceneElements = mapMaybe extractEvent (sceneElements scene)}
   in emap

addEventMap :: EventMap e -> ImRender e ()
addEventMap emap = ImRender $ do
  emref <- (.currSharedState.sharedEventMap) <$> ask
  liftIO $
    atomically $
      modifyTVar' emref (emap :)
