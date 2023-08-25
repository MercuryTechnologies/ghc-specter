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
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (fromBool, toBool)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Data.Map
  ( forwardLookup,
    keyMapToList,
    lookupKey,
  )
import GHCSpecter.Graphics.DSL
  ( Color (..),
    Scene (..),
    Stage (..),
  )
import GHCSpecter.Server.Types
  ( ServerState (..),
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
import ImGui qualified
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI
  ( defTableFlags,
    makeTabContents,
    windowFlagsNone,
    windowFlagsScroll,
  )
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

render :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
render ui ss = do
  renderState <- mkRenderState
  -- vec1 <- liftIO $ ImGui.newImVec2 0 0
  renderMainPanel tabs mconsoleFocus
  where
    -- liftIO $ delete vec1

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

renderMainContent = pure ()

renderMainPanel ::
  [(DriverId, Text)] ->
  Maybe DriverId ->
  ReaderT (SharedState UserEvent) IO ()
renderMainPanel tabs mconsoleFocus = do
  renderState <- mkRenderState
  whenM (toBool <$> liftIO (ImGui.beginTabBar ("#console-tabbar" :: CString))) $ do
    let tab_contents =
          fmap (\(drv_id, tab_title) -> (drv_id, tab_title, renderMainContent)) tabs
    _ <- makeTabContents tab_contents
    liftIO ImGui.endTabBar

{-
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
-}
