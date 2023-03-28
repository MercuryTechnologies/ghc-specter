{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM (
  atomically,
  newTChanIO,
  newTQueueIO,
  newTVar,
 )
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.Maybe (fromMaybe)
import GHCSpecter.Config (
  Config (..),
  defaultGhcSpecterConfigFile,
  loadConfig,
 )
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session.Types (ServerSession (..))
import GHCSpecter.Server.Types (initServerState)
import GI.Gtk qualified as Gtk

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #showAll mainWindow

  withConfig Nothing $ \cfg -> do
    let socketFile = configSocket cfg
        nodeSizeLimit = configModuleClusterSize cfg
    ssRef <- atomically $ newTVar (initServerState nodeSizeLimit)
    workQ <- newTQueueIO
    chanSignal <- newTChanIO
    let servSess = ServerSession ssRef chanSignal
    _ <- forkOS $ Comm.listener socketFile servSess workQ

    Gtk.main
