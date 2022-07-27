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
import qualified Data.Text as T
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
import Toolbox.Server.Types (Inbox)
import Prelude hiding (div)

main :: IO ()
main = do
  var <- atomically $ newTVar (0, mempty, SessionInfo Nothing)
  _ <- forkIO $ listener var
  webServer var

listener :: TVar (Int, Inbox, SessionInfo) -> IO ()
listener var =
  runServer
    "/tmp/ghc-build-analyzer.ipc"
    (\sock -> receiveObject sock >>= atomically . updateInbox var)

updateInbox :: TVar (Int, Inbox, SessionInfo) -> ChanMessageBox -> STM ()
updateInbox var chanMsg =
  modifyTVar' var $ \(i, m, s) ->
    case chanMsg of
      CMBox (CMCheckImports modu msg) ->
        (i + 1, M.insert (CheckImports, modu) msg m, s)
      CMBox (CMTiming modu timer') ->
        (i + 1, M.insert (Timing, modu) (T.pack (show timer')) m, s)
      CMBox (CMSession s') ->
        (i + 1, m, s')

webServer :: TVar (Int, Inbox, SessionInfo) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" $
    \_ -> loopM step ((CheckImports, Nothing), initVal)
  where
    step ((chan, mexpandedModu), (i, m, s)) = do
      let await = liftSTM $ do
            (i', m', s') <- readTVar var
            if i == i'
              then retry
              else pure (i', m', s')
      Left
        <$> ( render ((chan, mexpandedModu), (i, m, s))
                <|> (((chan, mexpandedModu),) <$> await)
            )
