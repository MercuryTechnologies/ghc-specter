{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Console
  ( render,
    consoleInputBufferSize,
  )
where

import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TF
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (copyBytes, fillBytes, fromBool, toBool)
import Foreign.Ptr (castPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Map
  ( KeyMap,
    keyMapToList,
    lookupKey,
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    ServerState (..),
  )
import GHCSpecter.UI.Console
  ( buildConsoleMain,
    getTabName,
  )
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types
  ( ConsoleUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui qualified
import ImGui.Enum
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import STD.Deletable (delete)
import Util.GUI
  ( makeTabContents,
    windowFlagsScroll,
  )
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    mkRenderState,
    renderConsoleItem,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  renderMainPanel ss tabs consoleMap mconsoleFocus inputEntry
  where
    pausedMap = ss._serverPaused
    consoleMap = ss._serverConsole
    mconsoleFocus = ui._uiModel._modelConsole._consoleFocus
    inputEntry = ui._uiModel._modelConsole._consoleInputEntry
    tabs = fmap (\(k, _) -> (k, getTabName ss k)) . keyMapToList $ pausedMap

renderMainContent ::
  ServerState ->
  KeyMap DriverId [ConsoleItem] ->
  Maybe DriverId ->
  Text ->
  ReaderT (SharedState UserEvent) IO ()
renderMainContent ss consoleMap mconsoleFocus inputEntry = do
  zerovec <- liftIO $ ImGui.newImVec2 0 0
  -- main contents
  vec2 <- liftIO $ ImGui.newImVec2 0 (-25)
  _ <- liftIO $ ImGui.beginChild ("console-main" :: CString) vec2 (fromBool True) windowFlagsScroll
  let items = buildConsoleMain consoleMap mconsoleFocus
  renderState <- mkRenderState
  liftIO $
    traverse_ (renderConsoleItem renderState.currSharedState) items
  {-
  let stage_ref = renderState.currSharedState.sharedStage
  Stage stage <- liftIO $ atomically $ readTVar stage_ref
  for_ (L.find ((== "console-main") . sceneId) stage) $ \stageMain -> do
    runImRender renderState $
      renderComponent
        True
        ConsoleEv
        ( do
            scene <- buildConsoleMain consoleMap mconsoleFocus
            pure
              scene
                { sceneGlobalViewPort = stageMain.sceneGlobalViewPort,
                  sceneLocalViewPort = stageMain.sceneLocalViewPort
                }
        )
  -}
  liftIO ImGui.endChild
  -- input text line
  renderInput inputEntry
  -- help window
  liftIO $ do
    v0 <- ImGui.getWindowPos
    w <- ImGui.getWindowWidth
    x0 <- imVec2_x_get v0
    y0 <- imVec2_y_get v0
    pos <- ImGui.newImVec2 (x0 + w - 150) (y0 + 60)
    ImGui.setNextWindowPos pos 0 zerovec
    liftIO $ delete pos
  vec1 <- liftIO $ ImGui.newImVec2 130 260
  _ <- liftIO $ ImGui.beginChild ("child_window" :: CString) vec1 (fromBool True) windowFlagsScroll
  renderHelp ss mconsoleFocus
  liftIO ImGui.endChild
  liftIO $ delete zerovec
  liftIO $ delete vec1
  liftIO $ delete vec2

renderMainPanel ::
  ServerState ->
  [(DriverId, Text)] ->
  KeyMap DriverId [ConsoleItem] ->
  Maybe DriverId ->
  Text ->
  ReaderT (SharedState UserEvent) IO ()
renderMainPanel ss tabs consoleMap mconsoleFocus inputEntry = do
  renderState <- mkRenderState
  whenM (toBool <$> liftIO (ImGui.beginTabBar ("#console-tabbar" :: CString))) $ do
    let tab_contents =
          fmap
            (\(drv_id, tab_title) -> (drv_id, tab_title, renderMainContent ss consoleMap mconsoleFocus inputEntry))
            tabs
    mselected <- makeTabContents Nothing tab_contents
    when (mconsoleFocus /= mselected) $
      case mselected of
        Nothing -> pure ()
        Just selected ->
          liftIO $ sendToControl (renderState.currSharedState) (ConsoleEv (ConsoleTab selected))
    liftIO ImGui.endTabBar

consoleInputBufferSize :: Int
consoleInputBufferSize = 4096

renderInput :: Text -> ReaderT (SharedState UserEvent) IO ()
renderInput inputEntry = do
  shared <- ask
  liftIO $
    TF.withCStringLen inputEntry $ \(p, len) -> do
      copyBytes (shared.sharedConsoleInput) p len
      pokeElemOff (shared.sharedConsoleInput) len 0
      let flags =
            fromIntegral $
              fromEnum ImGuiInputTextFlags_EnterReturnsTrue
                .|. fromEnum ImGuiInputTextFlags_CtrlEnterForNewLine
      vec <- ImGui.newImVec2 (-1) 24
      whenM
        ( toBool
            <$> ImGui.inputTextMultiline
              ("##command" :: CString)
              (shared.sharedConsoleInput)
              (fromIntegral consoleInputBufferSize)
              vec
              flags
        )
        $ do
          -- Process enter key pressed.
          inputEntry' <- TF.fromPtr0 (castPtr (shared.sharedConsoleInput))
          sendToControl shared (ConsoleEv (ConsoleInput inputEntry'))
          sendToControl shared (ConsoleEv (ConsoleKey "Enter"))
          fillBytes (castPtr (shared.sharedConsoleInput)) 0 consoleInputBufferSize
      delete vec

renderHelp :: ServerState -> Maybe DriverId -> ReaderT (SharedState UserEvent) IO ()
renderHelp ss mconsoleFocus = do
  case getHelp <$> mconsoleFocus of
    Nothing -> pure ()
    Just (title, items) -> do
      liftIO $ ImGui.textUnformatted (fromString (T.unpack title) :: CString)
      traverse_ renderItem items
  where
    renderItem (Left (txt, ev)) =
      whenM (toBool <$> liftIO (ImGui.button (fromString (T.unpack txt) :: CString))) $ do
        renderState <- mkRenderState
        liftIO $ sendToControl (renderState.currSharedState) (ConsoleEv ev)
    renderItem (Right txt) =
      liftIO $ ImGui.textUnformatted (fromString (T.unpack txt) :: CString)

    pausedMap = ss._serverPaused
    getHelp k =
      let title =
            let mpaused = lookupKey k pausedMap
             in maybe "" (\loc -> "paused at " <> T.pack (show loc)) mpaused
          classify txt =
            if txt == ":next" || txt == ":goto-source" || txt == ":dump-heap" || txt == ":exit-ghc-debug" || txt == ":list-core"
              then Left (txt, ConsoleButtonPressed True txt)
              else Right txt
          helpMsgs =
            maybe
              [Right "No Help!"]
              (fmap classify)
              (consoleCommandList <$> lookupKey k (ss._serverPaused))
       in (title, helpMsgs)
