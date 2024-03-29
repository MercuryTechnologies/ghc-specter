{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Render
  ( -- * state
    SharedState (..),
    ImRenderState (..),
    mkRenderState,

    -- * fonts
    c_detectScaleFactor,
    loadAllFonts,

    -- * ImRender monad
    ImRender (..),
    runImRender,

    -- * coord
    toGlobalCoords,
    fromGlobalCoords,

    -- * rendering and event map
    renderShape,
    renderPrimitive,
    renderScene,
    buildEventMap,
    addEventMap,

    -- * rendering console
    renderConsoleItem,

    -- * render dialog
    renderDialog,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    writeTQueue,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.Extra (ifM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..), get, modify')
import Data.ByteString (useAsCString)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Foreign qualified as T
import Data.Traversable (for)
import Data.Tree (drawTree)
import FFICXX.Runtime.Cast (FPtr (cast_fptr_to_obj))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
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
import GHCSpecter.Server.Types (ConsoleItem (..))
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    Tab (..),
    UserEvent (..),
  )
import ImGui
import ImGui.Enum
  ( ImGuiCond_ (..),
    ImGuiWindowFlags_ (..),
  )
import ImGui.ImFont.Implementation (imFont_Scale_set)
import STD.Deletable (delete)
import Util.Color (getNamedColor)
import Util.GUI (getOriginInImGui)
import Util.Orphans ()

--
-- state
--

-- TODO: Remove all TVars and purify this. (i.e. use a proper state monad).

data SharedState e = SharedState
  { sharedMousePos :: Maybe (Int, Int),
    sharedMouseWheel :: (Double, Double),
    sharedCtrlDown :: Bool,
    sharedIsMouseMoved :: Bool,
    sharedIsClicked :: Bool,
    sharedTabState :: Maybe Tab,
    -- TODO: this is impure. should be out of the pure state.
    sharedChanQEv :: TQueue Event,
    sharedFontsSans :: NonEmpty (Int, ImFont),
    sharedFontsMono :: NonEmpty (Int, ImFont),
    sharedFontScaleFactor :: Double,
    sharedEventMap :: [EventMap e],
    sharedStage :: Stage,
    -- TODO: this is impure.
    sharedConsoleInput :: CString,
    -- TODO: need to make a proper window config type.
    sharedWillScrollDownConsole :: Bool,
    sharedLeftPaneSize :: (Double, Double),
    sharedPopup1 :: Bool,
    sharedPopup2 :: Bool
  }

data ImRenderState = ImRenderState
  { -- TODO: this is impure.
    currDrawList :: ImDrawList,
    currOriginInImGui :: (Double, Double),
    currUpperLeftInGlobalViewport :: (Double, Double),
    currUpperLeftInLocalViewport :: (Double, Double),
    -- | (scaleX, scaleY)
    currScale :: (Double, Double),
    -- TODO: This is ugly. let's get rid of all these TVar, when changing to state monad.
    currLocalIDRef :: TVar Int
  }

mkRenderState :: StateT (SharedState e) IO ImRenderState
mkRenderState = do
  draw_list <- liftIO getWindowDrawList
  oxy <- liftIO getOriginInImGui
  local_id_ref <- liftIO $ newTVarIO 0
  pure
    ImRenderState
      { currDrawList = draw_list,
        currOriginInImGui = oxy,
        currUpperLeftInGlobalViewport = (0, 0),
        currUpperLeftInLocalViewport = (0, 0),
        currScale = (1.0, 1.0),
        currLocalIDRef = local_id_ref
      }

--
-- Font
--

#ifdef __MACOS__
foreign import ccall unsafe "detectScaleFactor"
  c_detectScaleFactor :: IO CFloat
#else
c_detectScaleFactor :: IO CFloat
c_detectScaleFactor = pure 1.0
#endif

fontSizeList :: NonEmpty Int
fontSizeList =
  6 :| [7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 24, 30, 36, 42, 48, 60, 72]

loadAllFonts :: FilePath -> ImFontAtlas -> CFloat -> IO (NonEmpty (Int, ImFont))
loadAllFonts path fonts scale_factor =
  for fontSizeList $ \i ->
    withCString path $ \cstr -> do
      font <- imFontAtlas_AddFontFromFileTTF fonts cstr (fromIntegral i * scale_factor)
      pure (i, font)

pickNearestFont :: NonEmpty (Int, ImFont) -> Double -> (Int, ImFont)
pickNearestFont fonts size =
  let (_smaller, bigger) = NE.break (\(i, _) -> (fromIntegral i >= size)) fonts
   in case bigger of
        [] -> NE.last fonts
        (x : _) -> x

--
-- ImRender monad
--

newtype ImRender e a = ImRender
  { unImRender :: ReaderT ImRenderState (StateT (SharedState e) IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ImRenderState)

runImRender :: ImRenderState -> ImRender e a -> StateT (SharedState e) IO a
runImRender s action = runReaderT (unImRender action) s

--
--
--

mkImVec2 :: (Double, Double) -> IO ImVec2
mkImVec2 (x, y) = newImVec2 (realToFrac x) (realToFrac y)

toGlobalCoords :: ImRenderState -> (Double, Double) -> (Double, Double)
toGlobalCoords s (x, y) =
  let (ox, oy) = s.currOriginInImGui
      (cx, cy) = s.currUpperLeftInGlobalViewport
      (vx, vy) = s.currUpperLeftInLocalViewport
      (sx, sy) = s.currScale
   in (ox + cx + sx * (x - vx), oy + cy + sy * (y - vy))

fromGlobalCoords :: ImRenderState -> (Double, Double) -> (Double, Double)
fromGlobalCoords s (x', y') =
  let (ox, oy) = s.currOriginInImGui
      (cx, cy) = s.currUpperLeftInGlobalViewport
      (vx, vy) = s.currUpperLeftInLocalViewport
      (sx, sy) = s.currScale
   in ((x' - ox - cx) / sx + vx, (y' - oy - cy) / sy + vy)

--
--

renderShape :: Shape -> ImRender e ()
renderShape (SRectangle (Rectangle (x, y) w h mline mbkg mlwidth)) = ImRender $ do
  s <- ask
  let (x', y') = toGlobalCoords s (x, y)
      (sx, sy) = s.currScale
  liftIO $ do
    v1 <- mkImVec2 (x', y')
    v2 <- mkImVec2 (x' + w * sx, y' + h * sy)
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
    let (x0', y0') = toGlobalCoords s xy0
        (x1', y1') = toGlobalCoords s xy1
        nPoints = length xys + 2
    -- TODO: make a utility function for this tedious and error-prone process
    allocaArray nPoints $ \(pp :: Ptr ImVec2) -> do
      p0 <- mkImVec2 (x0', y0')
      pokeElemOff pp 0 p0
      delete p0
      p1 <- mkImVec2 (x1', y1')
      pokeElemOff pp (nPoints - 1) p1
      delete p1
      for_ (zip [1 ..] xys) $ \(i, (x, y)) -> do
        let (x', y') = toGlobalCoords s (x, y)
        p <- mkImVec2 (x', y')
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
  shared <- lift get
  liftIO $ do
    let (_, sy) = s.currScale
        (selected_font_size, selected_font) =
          case font of
            Sans -> pickNearestFont shared.sharedFontsSans (fromIntegral fontSize * sy * scale_factor)
            Mono -> pickNearestFont shared.sharedFontsMono (fromIntegral fontSize * sy * scale_factor)
        scale_factor = shared.sharedFontScaleFactor
        factor = (fromIntegral fontSize) * sy / fromIntegral selected_font_size
    imFont_Scale_set selected_font (realToFrac factor)
    pushFont selected_font
    let offsetY = case pos of
          UpperLeft -> 0
          LowerLeft -> -fontSize
        x' = x
        y' = y + fromIntegral offsetY
        (x'', y'') = toGlobalCoords s (x', y')
    v' <- mkImVec2 (x'', y'')
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
      overlapCheck p =
        vp `overlapsWith` primBoundingBox p
      filtered = filter overlapCheck (sceneElements scene)
  local
    ( \s ->
        s
          { currUpperLeftInGlobalViewport = (cx0, cy0),
            currUpperLeftInLocalViewport = (vx0, vy0),
            currScale = (scaleX, scaleY)
          }
    )
    $ do
      s' <- ask
      let isValid = (vx0 < vx1) && (vy0 < vy1)
      when isValid $ do
        v1 <- liftIO $ mkImVec2 (toGlobalCoords s' (vx0, vy0))
        v2 <- liftIO $ mkImVec2 (toGlobalCoords s' (vx1, vy1))
        liftIO $ pushClipRect v1 v2 (fromBool True)
        liftIO $ delete v1
        liftIO $ delete v2
      traverse_ renderPrimitive filtered
      when isValid $
        liftIO popClipRect

buildEventMap :: Scene (Primitive e) -> EventMap e
buildEventMap scene =
  let -- TODO: handle events for other shapes
      extractEvent (Primitive (SRectangle (Rectangle (x, y) w h _ _ _)) _ (Just hitEvent)) =
        Just (hitEvent, ViewPort (x, y) (x + w, y + h))
      extractEvent _ = Nothing
      emap = scene {sceneElements = mapMaybe extractEvent (sceneElements scene)}
   in emap

addEventMap :: EventMap e -> ImRender e ()
addEventMap emap = ImRender $
  lift $
    modify' $
      \s ->
        let emap0 = s.sharedEventMap
         in s {sharedEventMap = emap : emap0}

--
-- console functions
--

--
renderConsoleItem :: forall e. ImRenderState -> ConsoleItem -> StateT (SharedState e) IO ()
renderConsoleItem _ (ConsoleCommand txt) = liftIO (separator >> T.withCString txt textUnformatted >> separator)
renderConsoleItem _ (ConsoleText txt) = liftIO $ T.withCString txt textUnformatted
renderConsoleItem s (ConsoleButton buttonss) = do
  shared <- get
  case NE.nonEmpty (mapMaybe NE.nonEmpty buttonss) of
    Nothing -> liftIO $ T.withCString "no buttons" textUnformatted
    Just ls' -> liftIO $ traverse_ (mkRow shared) ls'
  where
    mkButton shared (label, cmd) = do
      -- non-overlapping local id
      let ref = s.currLocalIDRef
      n <- atomically $ do
        n <- readTVar ref
        writeTVar ref (n + 1)
        pure n
      pushID (fromIntegral n)
      T.withCString label $ \cstr ->
        whenM (toBool <$> button cstr) $
          atomically $
            writeTQueue (shared.sharedChanQEv) (UsrEv (ConsoleEv (ConsoleButtonPressed False cmd)))
      popID
    mkRow :: SharedState e -> NonEmpty (Text, Text) -> IO ()
    mkRow shared buttons = do
      traverse_ (\itm -> mkButton shared itm >> sameLine_) buttons
      newLine
renderConsoleItem _ (ConsoleCore forest) = liftIO $ T.withCString (T.unlines $ fmap render1 forest) textUnformatted
  where
    render1 tr = T.pack $ drawTree $ fmap show tr

renderDialog :: Text -> Text -> (Bool -> SharedState e -> SharedState e) -> SharedState e -> IO (SharedState e)
renderDialog id' content setter shared =
  T.withCString id' $ \c_id ->
    T.withCString content $ \c_content -> do
      viewport <- getMainViewport
      openPopup c_id 0
      center <- imGuiViewport_GetCenter viewport
      rel_pos <- newImVec2 0.5 0.5
      setNextWindowPos center (fromIntegral (fromEnum ImGuiCond_Appearing)) rel_pos
      delete rel_pos
      let flag = fromIntegral (fromEnum ImGuiWindowFlags_AlwaysAutoResize)
      ifM
        (toBool <$> beginPopupModal c_id nullPtr flag)
        ( do
            textUnformatted c_content
            isClosePressed <- toBool <$> button ("close" :: CString)
            let shared'
                  | isClosePressed = setter False shared
                  | otherwise = shared
            endPopup
            pure shared'
        )
        (pure shared)
