{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Console
  ( render,
  )
where

import Control.Concurrent.STM
  ( atomically,
    readTVar,
    writeTQueue,
  )
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Bits ((.|.))
import Data.Foldable (for_, traverse_)
import Data.List qualified as L
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TF
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Array (callocArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (pokeElemOff)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Map
  ( KeyMap,
    forwardLookup,
    keyMapToList,
    lookupKey,
  )
import GHCSpecter.Graphics.DSL
  ( Color (..),
    Scene (..),
    Stage (..),
  )
import GHCSpecter.Server.Types
  ( ConsoleItem (..),
    ServerState (..),
  )
import GHCSpecter.UI.Console
  ( buildConsoleHelp,
    buildConsoleInput,
    buildConsoleMain,
    buildConsoleTab,
  )
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types
  ( ConsoleUI (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    UserEvent (..),
  )
import Handler (sendToControl)
import ImGui qualified
import ImGui.Enum
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI
  ( defTableFlags,
    makeTabContents,
    windowFlagsNone,
    windowFlagsScroll,
  )
import Util.Render
  ( ImRenderState (..),
    SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  renderState <- mkRenderState
  renderMainPanel ss tabs consoleMap mconsoleFocus
  renderInput inputEntry
  where
    pausedMap = ss._serverPaused
    consoleMap = ss._serverConsole
    mconsoleFocus = ui._uiModel._modelConsole._consoleFocus
    inputEntry = ui._uiModel._modelConsole._consoleInputEntry
    -- TODO: refactor this out and this should be out of Render.*
    getTabName k =
      let ktxt = T.pack $ show (unDriverId k)
          mlookedup = forwardLookup k (ss._serverDriverModuleMap)
       in maybe ktxt (\m -> ktxt <> " - " <> m) mlookedup
    tabs = fmap (\(k, _) -> (k, getTabName k)) . keyMapToList $ pausedMap

renderMainContent ::
  ServerState ->
  KeyMap DriverId [ConsoleItem] ->
  Maybe DriverId ->
  ReaderT (SharedState UserEvent) IO ()
renderMainContent ss consoleMap mconsoleFocus = do
  renderState <- mkRenderState
  zerovec <- liftIO $ ImGui.newImVec2 0 0
  vec1 <- liftIO $ ImGui.newImVec2 130 260
  liftIO $
    runImRender renderState $
      renderComponent ConsoleEv (buildConsoleMain consoleMap mconsoleFocus)
  liftIO $ do
    v0 <- ImGui.getWindowPos
    w <- ImGui.getWindowWidth
    x0 <- imVec2_x_get v0
    y0 <- imVec2_y_get v0
    pos <- ImGui.newImVec2 (x0 + w - 150) (y0 + 60)
    ImGui.setNextWindowPos pos 0 zerovec
    liftIO $ delete pos
  _ <- liftIO $ ImGui.beginChild ("child_window" :: CString) vec1 (fromBool True) windowFlagsScroll
  renderHelp ss mconsoleFocus
  liftIO ImGui.endChild
  liftIO $ delete zerovec

renderMainPanel ::
  ServerState ->
  [(DriverId, Text)] ->
  KeyMap DriverId [ConsoleItem] ->
  Maybe DriverId ->
  ReaderT (SharedState UserEvent) IO ()
renderMainPanel ss tabs consoleMap mconsoleFocus = do
  renderState <- mkRenderState
  whenM (toBool <$> liftIO (ImGui.beginTabBar ("#console-tabbar" :: CString))) $ do
    let tab_contents =
          fmap (\(drv_id, tab_title) -> (drv_id, tab_title, renderMainContent ss consoleMap mconsoleFocus)) tabs
    mselected <- makeTabContents tab_contents
    when (mconsoleFocus /= mselected) $
      case mselected of
        Nothing -> pure ()
        Just selected ->
          liftIO $ sendToControl (renderState.currSharedState) (ConsoleEv (ConsoleTab selected))
    liftIO ImGui.endTabBar

renderInput :: Text -> ReaderT (SharedState UserEvent) IO ()
renderInput inputEntry = do
  shared <- ask
  -- TODO: as an external parameter
  let bufSize = 4096
  liftIO $
    TF.withCStringLen inputEntry $ \(p, len) -> do
      copyBytes (shared.sharedConsoleInput) p len
      pokeElemOff (shared.sharedConsoleInput) len 0
      let flags =
            fromIntegral $
              fromEnum ImGuiInputTextFlags_None -- CallbackEdit
      whenM (toBool <$> ImGui.inputText ("##" :: CString) (shared.sharedConsoleInput) bufSize flags) $ do
        -- TODO: This is too hacky.
        print inputEntry
        inputEntry' <- TF.fromPtr0 (castPtr (shared.sharedConsoleInput))
        print inputEntry'
        when (inputEntry /= inputEntry') $
          sendToControl shared (ConsoleEv (ConsoleInput inputEntry'))
        pure ()

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
    consoleMap = ss._serverConsole
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
