module Control (
  ControlF (..),
  Control,
  updateState,
  updateView,
  nextEvent,
  stepControl,
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Lens ((^.))
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Types (
  CEvent (..),
  HasLogcatView (..),
  LogcatState,
  LogcatView,
 )

data ControlF r
  = UpdateState (LogcatState -> (Bool, LogcatState)) (Bool -> r)
  | UpdateView r
  | NextEvent (CEvent -> r)
  deriving (Functor)

type Control = Free ControlF

-- | update state and notify if it needs GUI update
updateState :: (LogcatState -> (Bool, LogcatState)) -> Control Bool
updateState upd = liftF (UpdateState upd id)

updateView :: Control ()
updateView = liftF (UpdateView ())

nextEvent :: Control CEvent
nextEvent = liftF (NextEvent id)

stepControl ::
  Control r ->
  ReaderT (MVar CEvent, TVar LogcatState, LogcatView) IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (UpdateState upd cont)) = do
  (_, ref, _) <- ask
  shouldUpdate <-
    liftIO $ atomically $ do
      s <- readTVar ref
      let (shouldUpdate, s') = upd s
      s' `seq` writeTVar ref s'
      pure shouldUpdate
  pure (Left (cont shouldUpdate))
stepControl (Free (UpdateView next)) = do
  (_, _, view) <- ask
  liftIO (view ^. logcatViewUpdater)
  pure (Left next)
stepControl (Free (NextEvent cont)) = do
  (lock, _, _) <- ask
  ev <- liftIO $ takeMVar lock
  pure (Left (cont ev))
