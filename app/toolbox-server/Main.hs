{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (liftSTM)
import Concur.Replica (runDefault)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
  )
import Control.Monad.Extra (loopM)
import qualified Data.Map.Strict as M
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

listener :: FilePath -> TVar ServerState -> IO ()
listener socketFile var =
  runServer
    socketFile
    (\sock -> receiveObject sock >>= atomically . updateInbox var)

updateInbox :: TVar ServerState -> ChanMessageBox -> STM ()
updateInbox var chanMsg =
  modifyTVar' var $ \ss ->
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
  let initUIState = UIState CheckImports Nothing
  runDefault 8080 "test" $
    \_ -> loopM step (initUIState, initServerState)
  where
    step (ui, ss) = do
      let await = liftSTM $ do
            ss' <- readTVar var
            if serverMessageSN ss == serverMessageSN ss'
              then retry
              else pure ss'
      (Left . (,ss) <$> render (ui, ss))
        <|> (Left . (ui,) <$> await)
