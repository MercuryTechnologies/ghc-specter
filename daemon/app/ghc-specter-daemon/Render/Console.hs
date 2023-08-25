{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Console
  ( renderConsole,
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
import qualified ImGui
import Render.Common (renderComponent)
import STD.Deletable (delete)
import Util.GUI
  ( defTableFlags,
    windowFlagsNone,
    windowFlagsScroll,
  )
import Util.Render
  ( SharedState (..),
    mkRenderState,
    runImRender,
  )

renderConsole :: UIState -> ServerState -> ReaderT (SharedState UserEvent) IO ()
renderConsole ui ss = do
  renderState <- mkRenderState
  vec1 <- liftIO $ ImGui.newImVec2 800 0
  vec2 <- liftIO $ ImGui.newImVec2 100 0
  whenM (toBool <$> liftIO (ImGui.beginTable ("##console" :: CString) 2 defTableFlags)) $ do
    liftIO $ ImGui.tableSetupColumn_ ("#console-column" :: CString)
    liftIO $ ImGui.tableNextRow 0
    liftIO $ ImGui.tableSetColumnIndex 0
    --
    _ <- liftIO $ ImGui.beginChild ("#console-main" :: CString) vec1 (fromBool False) windowFlagsScroll
    liftIO $
      runImRender renderState $ do
        renderComponent ConsoleEv (buildConsoleTab tabs mconsoleFocus)
    liftIO ImGui.endChild
    --
    liftIO $ ImGui.tableSetColumnIndex 1
    {-_ <- liftIO $ ImGui.beginChild ("#console-help" :: CString) vec2 (fromBool False) windowFlagsNone
    liftIO $
      runImRender renderState $ do
        renderComponent ConsoleEv (buildConsoleHelp getHelp mconsoleFocus)
    liftIO ImGui.endChild -}
    --
    liftIO ImGui.endTable
  liftIO $ delete vec1
  liftIO $ delete vec2
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

{-  chanQEv <- (.sharedChanQEv) <$> ask
  liftIO $ do
    -- Buttons return true when clicked (most widgets return true when edited/activated)
    whenM (toBool <$> button (":focus 1" :: CString)) $ do
      atomically $
        writeTQueue
          chanQEv
          (UsrEv (ConsoleEv (ConsoleTab (DriverId 1))))
    whenM (toBool <$> button (":next" :: CString)) $ do
      atomically $
        writeTQueue
          chanQEv
          (UsrEv (ConsoleEv (ConsoleButtonPressed True ":next")))
-}
