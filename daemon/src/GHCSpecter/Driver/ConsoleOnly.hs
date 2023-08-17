{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module GHCSpecter.Driver.ConsoleOnly
  ( main,
  )
where

-- import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TQueue,
    atomically,
    writeTQueue,
  )
-- import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Data.Text qualified as T
import GHCSpecter.Driver.Session.Types (ClientSession (..))
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    UserEvent (..),
  )
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

-- | This console-only driver is only for developments and tests
main :: ClientSession -> IO ()
main cliSess = do
  let _uiRef = cliSess._csUIStateRef
      _chanState = cliSess._csPublisherState
      chanQEv = cliSess._csPublisherEvent
      loop :: InputT IO ()
      loop = do
        minput <- getInputLine "% "
        case minput of
          Nothing -> pure ()
          Just "quit" -> pure ()
          Just input -> do
            outputStrLn $ "Input was: " <> input
            lift $
              atomically $ do
                writeTQueue
                  chanQEv
                  (UsrEv (ConsoleEv (ConsoleInput (T.pack input))))
                writeTQueue
                  chanQEv
                  (UsrEv (ConsoleEv (ConsoleKey "Enter")))

            loop
  runInputT defaultSettings $ do
    lift $
      atomically $ do
        writeTQueue
          chanQEv
          (UsrEv (ConsoleEv (ConsoleTab 1)))
    loop
