{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Driver.Terminal
  ( main,
  )
where

import Control.Concurrent.STM
  ( atomically,
    writeTQueue,
  )
import Control.Monad.Trans.Class (lift)
import Data.List qualified as L
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId (..))
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
import Text.Read (readMaybe)

-- | This console-only driver is only for developments and tests
main :: ClientSession -> IO ()
main cliSess = do
  let _uiRef = cliSess._csUIStateRef
      chanQEv = cliSess._csPublisherEvent
      loop :: InputT IO ()
      loop = do
        minput <- getInputLine "% "
        case minput of
          Nothing -> pure ()
          Just "quit" -> pure ()
          Just input
            | input == ":dump-timing" -> do
                outputStrLn $ "Input was: " <> input
                lift $
                  atomically $
                    writeTQueue
                      chanQEv
                      (UsrEv (ConsoleEv ConsoleDumpTiming))
                loop
            | input == ":dump-memory" -> do
                outputStrLn $ "Input was: " <> input
                lift $
                  atomically $
                    writeTQueue
                      chanQEv
                      (UsrEv (ConsoleEv ConsoleDumpMemory))
                loop
            | input == ":dump-modgraph" -> do
                outputStrLn $ "Input was: " <> input
                lift $
                  atomically $
                    writeTQueue
                      chanQEv
                      (UsrEv (ConsoleEv ConsoleDumpModGraph))
                loop
            | ":focus " `L.isPrefixOf` input -> do
                outputStrLn $ "Input was: " <> input
                let mx :: Maybe Int = readMaybe (drop 7 input)
                case mx of
                  Nothing -> outputStrLn "cannot parse the driver id"
                  Just x -> do
                    lift $
                      atomically $
                        writeTQueue
                          chanQEv
                          (UsrEv (ConsoleEv (ConsoleTab (DriverId x))))
                loop
            | otherwise -> do
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
    loop
