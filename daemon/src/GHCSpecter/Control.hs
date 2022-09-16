module GHCSpecter.Control
  ( Control,
    type Runner,
    control,
    stepControl,
    stepControlUpToEvent,
  )
where

import Concur.Core (Widget, unsafeBlockingIO)
import Control.Lens ((.~), (^.), _1, _2)
import Control.Monad (forever)
import Control.Monad.Extra (loopM)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), get, modify', put)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock qualified as Clock
import GHCSpecter.Channel (SessionInfo (..))
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIState (..),
    ModuleGraphUI (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( Event (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    TimingEvent (..),
  )
import System.IO (IOMode (..), withFile)
import System.IO.Unsafe (unsafePerformIO)

data ControlF r
  = GetState ((UIState, ServerState) -> r)
  | PutState (UIState, ServerState) r
  | NextEvent (Event -> r)
  | PrintMsg Text r
  | GetCurrentTime (UTCTime -> r)
  | GetLastUpdatedUI (UTCTime -> r)
  | ShouldUpdate Bool r
  | SaveSession r
  deriving (Functor)

type Control = Free ControlF

getState :: Control (UIState, ServerState)
getState = liftF (GetState id)

putState :: (UIState, ServerState) -> Control ()
putState (ui, ss) = liftF (PutState (ui, ss) ())

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

saveSession :: Control ()
saveSession = liftF (SaveSession ())

-- TODO: remove this
tempRef :: IORef Int
tempRef = unsafePerformIO (newIORef 0)
{-# NOINLINE tempRef #-}

type Runner = StateT (UIState, ServerState) (Widget IHTML)

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
  Control r ->
  -- | What the result means:
  -- Left _: continuation in the inner loop.
  -- Right (Left _): continuation that waits for a new event in the outer loop
  -- Right (Right _): final result as the business logic reaches its end.
  -- TODO: Use more descriptive custom types.
  Runner
    ( Either
        (Control r)
        ( Either
            (Event -> Control r)
            r
        )
    )
stepControl (Pure r) = pure (Right (Right r))
stepControl (Free (GetState cont)) = do
  (ui, ss) <- get
  pure (Left (cont (ui, ss)))
stepControl (Free (PutState (ui, ss) next)) = do
  put (ui, ss)
  pure (Left next)
stepControl (Free (NextEvent cont)) =
  pure (Right (Left cont))
stepControl (Free (PrintMsg txt next)) = do
  lift $
    unsafeBlockingIO $ do
      n <- readIORef tempRef
      modifyIORef' tempRef (+ 1)
      TIO.putStrLn $ (T.pack (show n) <> " : " <> txt)
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
stepControl (Free (SaveSession next)) = do
  (_, ss) <- get
  -- TODO: use asynchronous worker
  liftIO $
    withFile "session.json" WriteMode $ \h ->
      BL.hPutStr h (encode ss)
  pure (Left next)

-- | The inner loop described in the Note [Control Loops].
stepControlUpToEvent ::
  Event ->
  (Event -> Control r) ->
  Runner (Either (Event -> Control r) r)
stepControlUpToEvent ev cont0 = loopM stepControl (cont0 ev)

uiUpdateInterval :: NominalDiffTime
uiUpdateInterval = Clock.secondsToNominalDiffTime (fromRational (1 / 10))

handleEvent :: Event -> UTCTime -> Control ()
handleEvent topEv stepStartTime = do
  (oldUI, oldSS) <- getState
  (newUI, newSS) <- handleMainEvent (oldUI, oldSS)
  putState (newUI, newSS)
  where
    handleMainEvent (oldUI, oldSS) =
      case topEv of
        TabEv tab' -> do
          let newUI = (uiTab .~ tab') oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        ExpandModuleEv mexpandedModu' -> do
          let newUI = (uiSourceView . srcViewExpandedModule .~ mexpandedModu') oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        MainModuleEv ev -> do
          let mgui = oldUI ^. uiMainModuleGraph
          (mgui', mxy) <- handleModuleGraphEv ev mgui
          let newUI = (uiMainModuleGraph .~ mgui') oldUI
              newUI' = handleMouseMove newUI mxy
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI', newSS)
        SubModuleEv sev ->
          case sev of
            SubModuleGraphEv ev -> do
              let mgui = oldUI ^. uiSubModuleGraph . _2
              (mgui', mxy) <- handleModuleGraphEv ev mgui
              let newUI = (uiSubModuleGraph . _2 .~ mgui') oldUI
                  newUI' = handleMouseMove newUI mxy
                  newSS = (serverShouldUpdate .~ False) oldSS
              pure (newUI', newSS)
            SubModuleLevelEv d' -> do
              let newUI = (uiSubModuleGraph . _1 .~ d') oldUI
                  newSS = (serverShouldUpdate .~ False) oldSS
              pure (newUI, newSS)
        SessionEv SaveSessionEv -> do
          saveSession
          let newSS = (serverShouldUpdate .~ False) oldSS
          pure (oldUI, newSS)
        SessionEv ResumeSessionEv -> do
          let sinfo = oldSS ^. serverSessionInfo
              sinfo' = sinfo {sessionIsPaused = False}
              newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
          pure (oldUI, newSS)
        SessionEv PauseSessionEv -> do
          let sinfo = oldSS ^. serverSessionInfo
              sinfo' = sinfo {sessionIsPaused = True}
              newSS = (serverSessionInfo .~ sinfo') . (serverShouldUpdate .~ True) $ oldSS
          pure (oldUI, newSS)
        TimingEv (UpdateSticky b) -> do
          let newUI = (uiTiming . timingUISticky .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        TimingEv (UpdatePartition b) -> do
          let newUI = (uiTiming . timingUIPartition .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        TimingEv (UpdateParallel b) -> do
          let newUI = (uiTiming . timingUIHowParallel .~ b) oldUI
              newSS = (serverShouldUpdate .~ False) oldSS
          pure (newUI, newSS)
        MessageChanUpdated -> do
          let newSS = (serverShouldUpdate .~ True) oldSS
          pure (oldUI, newSS)

    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      Control (ModuleGraphUI, Maybe (UTCTime, (Double, Double)))
    handleModuleGraphEv (HoverOnModuleEv mhovered) mgui =
      pure ((modGraphUIHover .~ mhovered) mgui, Nothing)
    handleModuleGraphEv (ClickOnModuleEv mclicked) mgui =
      pure ((modGraphUIClick .~ mclicked) mgui, Nothing)
    handleModuleGraphEv (DummyEv mxy) mgui = do
      t <- getCurrentTime
      printMsg (T.pack (show (stepStartTime, t, mxy)))
      pure (mgui, (t,) <$> mxy)

    handleMouseMove ::
      UIState ->
      Maybe (UTCTime, (Double, Double)) ->
      UIState
    handleMouseMove ui_ mtxy =
      case mtxy of
        Nothing -> ui_
        Just (t, xy) ->
          if ui_ ^. uiShouldUpdate
            then (uiLastUpdated .~ t) . (uiMousePosition .~ xy) $ ui_
            else uiMousePosition .~ xy $ ui_

control :: Control ()
control = forever $ do
  lastUpdatedUI <- getLastUpdatedUI
  stepStartTime <- getCurrentTime

  -- wait for update interval, not to have too frequent update
  if (stepStartTime `Clock.diffUTCTime` lastUpdatedUI > uiUpdateInterval)
    then shouldUpdate True
    else shouldUpdate False

  printMsg "waiting for the next event"
  ev <- nextEvent
  printMsg (T.pack (show ev))
  handleEvent ev stepStartTime
