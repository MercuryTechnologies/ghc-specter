{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (liftSTM)
import Concur.Replica (runDefault)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
  )
import Control.Monad (void, when)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
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
    incrementSN,
    initServerState,
    initUIState,
  )
import Toolbox.Worker (moduleGraphWorker)
import Prelude hiding (div)

data CLIMode
  = Online FilePath
  | View FilePath

onlineMode :: OA.Mod OA.CommandFields CLIMode
onlineMode =
  OA.command
    "online"
    ( OA.info
        (Online <$> OA.strOption (OA.long "socket-file" <> OA.short 's' <> OA.help "socket file"))
        (OA.progDesc "GHC inspection on the fly")
    )

viewMode :: OA.Mod OA.CommandFields CLIMode
viewMode =
  OA.command
    "view"
    ( OA.info
        (View <$> OA.strOption (OA.long "session-file" <> OA.short 'f' <> OA.help "session file"))
        (OA.progDesc "viewing saved session")
    )

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info (OA.subparser (onlineMode <> viewMode) OA.<**> OA.helper) OA.fullDesc

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Online socketFile -> do
      var <- atomically $ newTVar initServerState
      _ <- forkIO $ listener socketFile var
      webServer var
    View sessionFile -> do
      lbs <- BL.readFile sessionFile
      case eitherDecode' lbs of
        Left err -> print err
        Right ss -> do
          var <- atomically $ newTVar ss
          webServer var

updateInterval :: NominalDiffTime
updateInterval = secondsToNominalDiffTime (fromRational (1 / 2))

listener :: FilePath -> TVar ServerState -> IO ()
listener socketFile var =
  runServer
    socketFile
    ( \sock -> do
        o <- receiveObject sock
        case o of
          CMBox (CMSession s') -> do
            let mgi = sessionModuleGraph s'
            void $ forkIO (moduleGraphWorker var mgi)
          _ -> pure ()
        atomically . modifyTVar' var . updateInbox $ o
    )

updateInbox :: ChanMessageBox -> ServerState -> ServerState
updateInbox chanMsg ss =
  incrementSN $
    case chanMsg of
      CMBox (CMTypeCheck modu msg) ->
        let m = serverInbox ss
         in ss {serverInbox = M.insert (TypeCheck, modu) msg m}
      CMBox (CMTiming modu timer') ->
        let m = serverTiming ss
         in ss {serverTiming = M.insert modu timer' m}
      CMBox (CMSession s') ->
        ss {serverSessionInfo = s'}

webServer :: TVar ServerState -> IO ()
webServer var = do
  ss0 <- atomically (readTVar var)
  initTime <- getCurrentTime
  runDefault 8080 "test" $
    \_ -> loopM step (initUIState, ss0, initTime)
  where
    step (ui, ss, lastUIUpdate) = do
      let await = do
            -- wait for update interval, not to have too frequent update
            currentTime_ <- liftIO getCurrentTime
            when (currentTime_ `diffUTCTime` lastUIUpdate < updateInterval) $
              liftIO $
                threadDelay (floor (nominalDiffTimeToSeconds updateInterval * 1_000_000))
            -- lock until new message comes
            ss' <- liftSTM $ do
              ss' <- readTVar var
              if (serverMessageSN ss == serverMessageSN ss')
                then retry
                else pure ss'
            newUIUpdate <- liftIO getCurrentTime
            pure (ui, ss', newUIUpdate)
      (Left . (,ss,lastUIUpdate) <$> render (ui, ss))
        <|> (Left <$> await)
