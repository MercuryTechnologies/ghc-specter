{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (liftSTM)
import Concur.Replica (runDefault)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    diffUTCTime,
    getCurrentTime,
    secondsToNominalDiffTime,
  )
import qualified Options.Applicative as OA
import Toolbox.Channel
  ( ChanMessage (..),
    ChanMessageBox (..),
    Channel (..),
    SessionInfo (..),
  )
import Toolbox.Comm
  ( receiveObject,
    runServer,
  )
import Toolbox.Render (render)
import Toolbox.Server.Types
  ( ServerState (..),
    UIState (..),
    incrementSN,
  )
import Prelude hiding (div)

newtype Options = Options {optSocketFile :: FilePath}

optsParser :: OA.ParserInfo Options
optsParser =
  OA.info
    (Options <$> OA.strOption (OA.long "socket-file" <> OA.short 's' <> OA.help "socket file"))
    OA.fullDesc

main :: IO ()
main = do
  opts <- OA.execParser optsParser
  let socketFile = optSocketFile opts
  var <- atomically $ newTVar (ServerState 0 mempty (SessionInfo Nothing) mempty)
  _ <- forkIO $ listener socketFile var
  webServer var

updateInterval :: NominalDiffTime
updateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

listener :: FilePath -> TVar ServerState -> IO ()
listener socketFile var =
  runServer
    socketFile
    (\sock -> receiveObject sock >>= atomically . modifyTVar' var . updateInbox)

updateInbox :: ChanMessageBox -> ServerState -> ServerState
updateInbox chanMsg ss =
  incrementSN $
    case chanMsg of
      CMBox (CMCheckImports modu msg) ->
        let m = serverInbox ss
         in ss {serverInbox = M.insert (CheckImports, modu) msg m}
      CMBox (CMTiming modu timer') ->
        let m = serverTiming ss
         in ss {serverTiming = M.insert modu timer' m}
      CMBox (CMSession s') ->
        ss {serverSessionInfo = s'}

webServer :: TVar ServerState -> IO ()
webServer var = do
  initServerState <- atomically (readTVar var)
  initTime <- getCurrentTime
  let initUIState = UIState CheckImports Nothing
  runDefault 8080 "test" $
    \_ -> loopM step (initUIState, initServerState, initTime)
  where
    step (ui, ss, lastUIUpdate) = do
      let await = do
            -- wait for update interval, not to have too frequent update
            currentTime_ <- liftIO getCurrentTime
            when (currentTime_ `diffUTCTime` lastUIUpdate < updateInterval) $
              liftIO $ threadDelay 500_000
            -- lock until new message comes
            ss' <-
              liftIO $
                atomically $ do
                  ss' <- readTVar var
                  if (serverMessageSN ss == serverMessageSN ss')
                    then retry
                    else pure ss'
            newUIUpdate <- liftIO getCurrentTime
            pure (ui, ss', newUIUpdate)
      (Left . (,ss,lastUIUpdate) <$> render (ui, ss))
        <|> (Left <$> await)
