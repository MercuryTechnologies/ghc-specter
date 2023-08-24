{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Console
  ( renderConsole,
  )
where

import Control.Concurrent.STM
  ( atomically,
    writeTQueue,
  )
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Foreign.C.String (CString)
import Foreign.Marshal.Utils (toBool)
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    UserEvent (..),
  )
import ImGui
import Util.Render (SharedState (..))

renderConsole :: ReaderT (SharedState UserEvent) IO ()
renderConsole = do
  chanQEv <- (.sharedChanQEv) <$> ask
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
