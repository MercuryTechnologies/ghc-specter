{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Control.Runner
  ( RunnerHandler (..),
    RunnerEnv (..),
    type Runner,
    stepControl,
    stepControlUpToEvent,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    TVar,
    atomically,
    modifyTVar',
    readTVar,
    writeTChan,
    writeTQueue,
    writeTVar,
  )
import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Control.Monad.Extra (loopM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Indexed.Free (IxFree (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
-- import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel.Inbound.Types (Request)
import GHCSpecter.Control.Types
  ( ControlF (..),
    type Control,
  )
import GHCSpecter.Graphics.DSL
  ( EventMap,
    Scene,
  )
import GHCSpecter.Server.Types (ServerState)
import GHCSpecter.UI.Types
  ( HasUIState (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( BackgroundEvent (..),
    Event (..),
    SystemEvent (..),
    UserEvent,
  )

-- import System.IO (IOMode (..), withFile)

data RunnerHandler e = RunnerHandler
  { runHandlerRefreshAction :: IO (),
    runHandlerHitScene ::
      (Double, Double) ->
      IO (Maybe (EventMap UserEvent)),
    runHandlerGetScene ::
      Text ->
      IO (Maybe (EventMap UserEvent)),
    runHandlerAddToStage :: Scene () -> IO (),
    runHandlerScrollDownConsoleToEnd :: IO ()
  }

-- | mutating state and a few handlers
data RunnerEnv e = RunnerEnv
  { runnerCounter :: IORef Int,
    runnerUIState :: TVar UIState,
    runnerServerState :: TVar ServerState,
    runnerQEvent :: TQueue Event,
    runnerSignalChan :: TChan Request,
    runnerHandler :: RunnerHandler e
  }

type Runner e = ReaderT (RunnerEnv e) IO

getUI' :: Runner e UIState
getUI' = do
  uiRef <- runnerUIState <$> ask
  liftIO $ atomically $ readTVar uiRef

putUI' :: UIState -> Runner e ()
putUI' ui = do
  uiRef <- runnerUIState <$> ask
  liftIO $ atomically $ writeTVar uiRef ui

modifyUI' :: (UIState -> UIState) -> Runner e ()
modifyUI' f = do
  uiRef <- runnerUIState <$> ask
  liftIO $ atomically $ modifyTVar' uiRef f

getSS' :: Runner e ServerState
getSS' = do
  ssRef <- runnerServerState <$> ask
  liftIO $ atomically $ readTVar ssRef

putSS' :: ServerState -> Runner e ()
putSS' ss = do
  ssRef <- runnerServerState <$> ask
  liftIO $ atomically $ writeTVar ssRef ss

modifySS' :: (ServerState -> ServerState) -> Runner e ()
modifySS' f = do
  ssRef <- runnerServerState <$> ask
  liftIO $ atomically $ modifyTVar' ssRef f

modifyUISS' ::
  ((UIState, ServerState) -> (UIState, ServerState)) ->
  Runner e ((UIState, ServerState), (UIState, ServerState))
modifyUISS' f = do
  (uiRef, ssRef) <- ((,) <$> runnerUIState <*> runnerServerState) <$> ask
  liftIO $ atomically $ do
    ui <- readTVar uiRef
    ss <- readTVar ssRef
    let (ui', ss') = f (ui, ss)
    ui' `seq` ss' `seq` (writeTVar uiRef ui' >> writeTVar ssRef ss')
    pure ((ui, ss), (ui', ss'))

sendRequest' :: Request -> Runner e ()
sendRequest' req = do
  signalChan <- runnerSignalChan <$> ask
  liftIO $ atomically $ writeTChan signalChan req

{-

Note [Control Loops]
~~~~~~~~~~~~~~~~~~~

Control monad is embedded DSL (eDSL) which describe the program logic of the ghc-specter
daemon. It is based on Free monad over a pattern functor that defines the allowed effects
of the program logic.

Among the effects, NextEvent is special since, at that step, the control runner yields
its operation to the underlying Widget rendering engine or message receiver until a new
event comes. Therefore, the control loop is defined in terms of 2-layered nested loops,
where the outer loop is to run the event poking step interleaved with inner loop operations,
and the inner loop is to process non-event-poking steps.

-}

-- | A single primitive step for the inner loop. See Note [Control Loops].
stepControl ::
  Control e r ->
  -- | What the result means:
  -- Left _: continuation in the inner loop.
  -- Right (Left _): continuation that waits for a new event in the outer loop
  -- Right (Right _): final result as the business logic reaches its end.
  -- TODO: Use more descriptive custom types.
  Runner
    e
    ( Either
        (Control e r)
        ( Either
            (Event -> Control e r)
            r
        )
    )
stepControl (Pure r) = pure (Right (Right r))
stepControl (Free (GetUI cont)) = do
  ui <- getUI'
  pure (Left (cont ui))
stepControl (Free (PutUI ui next)) = do
  putUI' ui
  pure (Left next)
stepControl (Free (ModifyUI upd next)) = do
  modifyUI' upd
  pure (Left next)
stepControl (Free (GetSS cont)) = do
  ss <- getSS'
  pure (Left (cont ss))
stepControl (Free (PutSS ss next)) = do
  putSS' ss
  pure (Left next)
stepControl (Free (ModifySS upd next)) = do
  modifySS' upd
  pure (Left next)
stepControl (Free (ModifyUISS upd next)) = do
  void $ modifyUISS' upd
  pure (Left next)
stepControl (Free (ModifyAndReturn upd cont)) = do
  (_before, after) <- modifyUISS' upd
  pure (Left (cont after))
stepControl (Free (ModifyAndReturnBoth upd cont)) = do
  (before, after) <- modifyUISS' upd
  pure (Left (cont (before, after)))
stepControl (Free (HitScene xy cont)) = do
  hitScene' <- runHandlerHitScene . runnerHandler <$> ask
  memap <- liftIO $ hitScene' xy
  pure (Left (cont memap))
stepControl (Free (GetScene name cont)) = do
  getScene' <- runHandlerGetScene . runnerHandler <$> ask
  memap <- liftIO $ getScene' name
  pure (Left (cont memap))
stepControl (Free (AddToStage scene next)) = do
  addToStage' <- runHandlerAddToStage . runnerHandler <$> ask
  liftIO $ addToStage' scene
  pure (Left next)
stepControl (Free (ScrollDownConsoleToEnd next)) = do
  scrollDownConsoleToEnd' <- runHandlerScrollDownConsoleToEnd . runnerHandler <$> ask
  liftIO scrollDownConsoleToEnd'
  pure (Left next)
stepControl (Free (SendRequest b next)) = do
  sendRequest' b
  pure (Left next)
stepControl (Free (NextEvent cont)) =
  pure (Right (Left cont))
stepControl (Free (PrintMsg txt next)) = do
  counterRef <- runnerCounter <$> ask
  liftIO $ do
    n <- readIORef counterRef
    modifyIORef' counterRef (+ 1)
    TIO.putStrLn $ (T.pack (show n) <> " : " <> txt)
  pure (Left next)
stepControl (Free (GetCurrentTime cont)) = do
  now <- liftIO Clock.getCurrentTime
  pure (Left (cont now))
stepControl (Free (GetLastUpdatedUI cont)) = do
  lastUpdatedUI <- (^. uiLastUpdated) <$> getUI'
  pure (Left (cont lastUpdatedUI))
stepControl (Free (ShouldUpdate b next)) = do
  modifyUI' (uiShouldUpdate .~ b)
  pure (Left next)
stepControl (Free (SaveSession next)) = do
  -- TODO: proper saving
  {- ss <- getSS'
  -- TODO: use asynchronous worker
  liftIO $
    withFile "session.json" WriteMode $ \h ->
      BL.hPutStr h (encode ss) -}
  pure (Left next)
stepControl (Free (Refresh next)) = do
  refreshAction <- runHandlerRefreshAction . runnerHandler <$> ask
  liftIO refreshAction
  pure (Left next)
stepControl (Free (RefreshUIAfter nSec next)) = do
  chanQEv <- runnerQEvent <$> ask
  liftIO $ do
    threadDelay (floor (nSec * 1_000_000))
    atomically $ writeTQueue chanQEv (SysEv (BkgEv RefreshUI))
  pure (Left next)
stepControl (Free (AsyncWork worker next)) = do
  ssRef <- runnerServerState <$> ask
  _ <- liftIO $ forkIO $ worker ssRef
  pure (Left next)

-- | The inner loop described in the Note [Control Loops].
stepControlUpToEvent ::
  Event ->
  (Event -> Control e r) ->
  Runner e (Either (Event -> Control e r) r)
stepControlUpToEvent ev cont0 = loopM stepControl (cont0 ev)
