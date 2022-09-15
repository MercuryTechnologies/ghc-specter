module GHCSpecter.Control
  ( Control,
    control,
    stepControl,
  )
where

import Concur.Core (Widget, unsafeBlockingIO)
import Control.Lens ((.~), (^.), _1)
import Control.Monad (forever)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), get, modify')
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock qualified as Clock
import GHCSpecter.Server.Types (ServerState (..))
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types (HasUIState (..), UIState)
import GHCSpecter.UI.Types.Event (Event)

data ControlF r
  = CommitUI UIState r
  | CommitServer ServerState r
  | NextEvent (Event -> r)
  | PrintMsg Text r
  | GetCurrentTime (UTCTime -> r)
  | GetLastUpdatedUI (UTCTime -> r)
  | ShouldUpdate Bool r
  deriving (Functor)

type Control = Free ControlF

commitUI :: UIState -> Control ()
commitUI ui = liftF (CommitUI ui ())

commitServer :: ServerState -> Control ()
commitServer ss = liftF (CommitServer ss ())

nextEvent :: Control Event
nextEvent = liftF (NextEvent id)

printMsg :: Text -> Control ()
printMsg txt = liftF (PrintMsg txt ())

getCurrentTime :: Control UTCTime
getCurrentTime = liftF (GetCurrentTime id)

getLastUpdatedUI :: Control UTCTime
getLastUpdatedUI = liftF (GetLastUpdatedUI id)

shouldUpdate :: Bool -> Control ()
shouldUpdate b = liftF (ShouldUpdate b ())

-- | step interpretation
stepControl :: Control r -> StateT (UIState, ServerState) (Widget IHTML) (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (CommitUI _ next)) = pure (Left next)
stepControl (Free (CommitServer _ next)) = pure (Left next)
stepControl (Free (NextEvent cont)) = pure (Left (cont undefined))
stepControl (Free (PrintMsg txt next)) = do
  lift $ unsafeBlockingIO (TIO.putStrLn txt)
  pure (Left next)
stepControl (Free (GetCurrentTime cont)) = do
  now <- lift $ unsafeBlockingIO Clock.getCurrentTime
  pure (Left (cont now))
stepControl (Free (GetLastUpdatedUI cont)) = do
  lastUpdatedUI <- (^. _1 . uiLastUpdated) <$> get
  pure (Left (cont lastUpdatedUI))
stepControl (Free (ShouldUpdate b next)) = do
  modify' (_1 . uiShouldUpdate .~ b)
  pure (Left next)

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = Clock.secondsToNominalDiffTime (fromRational (1 / 10))

control :: Control ()
control = forever $ do
  printMsg "control message"
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime

  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False
