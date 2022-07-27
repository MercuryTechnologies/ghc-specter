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
  ( ChanMessage (CMCheckImports, CMTiming),
    ChanMessageBox (..),
    Channel (..),
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
  var <- atomically $ newTVar (0, mempty)
  _ <- forkIO $ listener var
  webServer var

listener :: TVar (Int, Inbox) -> IO ()
listener var =
  runServer
    "/tmp/ghc-build-analyzer.ipc"
    (\sock -> receiveObject sock >>= atomically . updateInbox var)

updateInbox :: TVar (Int, Inbox) -> ChanMessageBox -> STM ()
updateInbox var chanMsg =
  modifyTVar' var $ \(i, m) ->
    let (chan, modu, msg) =
          case chanMsg of
            CMBox (CMCheckImports m' t') -> (CheckImports, m', t')
            CMBox (CMTiming m' timer') -> (Timing, m', T.pack (show timer'))
     in (i + 1, M.insert (chan, modu) msg m)

webServer :: TVar (Int, Inbox) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" $
    \_ -> loopM step ((CheckImports, Nothing), initVal)
  where
    step ((chan, mexpandedModu), (i, m)) = do
      let await = liftSTM $ do
            (i', m') <- readTVar var
            if i == i'
              then retry
              else pure (i', m')
      Left
        <$> ( render ((chan, mexpandedModu), (i, m))
                <|> (((chan, mexpandedModu),) <$> await)
            )
