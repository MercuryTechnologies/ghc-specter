{- FOURMOLU_DISABLE -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- NOTE: This disables the unwanted warning due to the use of forkPingThread.
-- TODO: use withPingThread as suggested after an investigation.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | This module is originated from Network.Wai.Handler.Replica.
-- When IHTML can be marked with the non-update (the Left case) directive,
-- the DOM diff updating is bypassed.
module GHCSpecter.ConcurReplica.WaiHandler
  ( Event (..),
    Callback (..),
    Context (..),
    app,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (SomeException (SomeException), evaluate, try)
import Control.Monad (forever, join)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Debug.Trace (traceIO)
import GHCSpecter.ConcurReplica.Types (IHTML (..))
import Network.HTTP.Types (status200)
import Network.Wai (Application, Middleware, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp)
import Network.WebSockets.Connection (Connection, ConnectionOptions, acceptRequest, forkPingThread, receiveData, sendClose, sendCloseCode, sendTextData)
import Replica.VDOM qualified as V
import Replica.VDOM.Render qualified as R

-- | Events are sent from the client to the server.
data Event
  = Event
      { evtType :: T.Text,
        evtEvent :: A.Value,
        evtPath :: [Int],
        evtClientFrame :: Int
      }
  | CallCallback A.Value Int
  deriving (Show)

instance A.FromJSON Event where
  parseJSON (A.Object o) = do
    t <- o .: "type"
    case (t :: T.Text) of
      "event" ->
        Event
          <$> o
          .: "eventType"
          <*> o
          .: "event"
          <*> o
          .: "path"
          <*> o
          .: "clientFrame"
      "call" ->
        CallCallback
          <$> o
          .: "arg"
          <*> o
          .: "id"
      _ -> fail "Expected \"type\" == \"event\" | \"call\""
  parseJSON _ = fail "Expected object"

-- | Updates are sent from the server to the client.
data Update
  = ReplaceDOM V.HTML
  | UpdateDOM Int (Maybe Int) [V.Diff]
  | Call
      A.Value
      -- ^ Argument to the call
      T.Text
      -- ^ Raw JS to be called

instance A.ToJSON Update where
  toJSON (ReplaceDOM dom) =
    A.object
      [ "type" .= V.t "replace",
        "dom" .= dom
      ]
  toJSON (UpdateDOM serverFrame clientFrame ddiff) =
    A.object
      [ "type" .= V.t "update",
        "serverFrame" .= serverFrame,
        "clientFrame" .= clientFrame,
        "diff" .= ddiff
      ]
  toJSON (Call arg js) =
    A.object
      [ "type" .= V.t "call",
        "arg" .= arg,
        "js" .= js
      ]

newtype Callback = Callback Int
  deriving (Eq, A.ToJSON, A.FromJSON)

-- | Context passed to the user's update function.
--
-- Use @call@ to run JS on the client.
--
-- To return a result from the client to the server,
-- first use @registerCallback@ to specify what's to be done with the result
-- and to get a 'Callback'.
--
-- Then pass that @Callback@ as the first argument to @call@.
-- Within the JS statement you also pass to call you'll have that argument
-- available as the variable @arg@, which you can use as follows:
--
-- > callCallback(arg, <data-to-pass-to-the-server>)
data Context = Context
  { registerCallback :: forall a. A.FromJSON a => (a -> IO ()) -> IO Callback,
    unregisterCallback :: Callback -> IO (),
    call :: forall a. A.ToJSON a => a -> T.Text -> IO ()
  }

app ::
  forall st.
  V.HTML ->
  ConnectionOptions ->
  Middleware ->
  st ->
  (Context -> st -> IO (Maybe (IHTML, st, Event -> Maybe (IO ())))) ->
  Application
app index options middleware initial step =
  websocketsOr options (websocketApp initial step) (middleware backupApp)
  where
    indexBS = BL.fromStrict $ TE.encodeUtf8 $ TL.toStrict $ TB.toLazyText $ R.renderHTML index

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [("content-type", "text/html")] indexBS

websocketApp ::
  forall st.
  st ->
  (Context -> st -> IO (Maybe (IHTML, st, Event -> Maybe (IO ())))) ->
  ServerApp
websocketApp initial step pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newTVarIO Nothing
  cf <- newTVarIO Nothing
  cbs <- newIORef (0, M.empty)

  let ctx =
        Context
          { registerCallback = \cb -> atomicModifyIORef' cbs $ \(cbId, cbs') ->
              ( ( cbId + 1,
                  flip (M.insert cbId) cbs' $ \arg -> case A.fromJSON arg of
                    A.Success arg' -> cb arg'
                    _ -> pure ()
                ),
                Callback cbId
              ),
            unregisterCallback = \(Callback cbId') -> atomicModifyIORef' cbs $ \(cbId, cbs') ->
              ((cbId, M.delete cbId' cbs'), ()),
            call = \arg js -> sendTextData conn $ A.encode $ Call (A.toJSON arg) js
          }

  forkPingThread conn 30

  tid <- forkIO $
    forever $ do
      msg <- receiveData conn
      case A.decode msg of
        Just msg' -> case msg' of
          Event {} -> do
            join $
              atomically $ do
                fire <- readTVar chan
                case fire of
                  Just fire' -> do
                    case fire' msg' of
                      Just io -> do
                        writeTVar chan Nothing
                        writeTVar cf (Just $ evtClientFrame msg')
                        pure io
                      Nothing -> pure (pure ())
                  Nothing -> retry
          CallCallback arg cbId -> do
            (_, cbs') <- readIORef cbs

            case M.lookup cbId cbs' of
              Just cb -> cb arg
              Nothing -> pure ()
        Nothing -> traceIO $ "Couldn't decode event: " <> show msg

  r <- try $ go conn ctx chan cf Nothing initial 0

  case r of
    Left (SomeException e) -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right _ -> sendClose conn ("done" :: T.Text)

  killThread tid
  where
    closeCodeInternalError = 1011

    go ::
      Connection ->
      Context ->
      TVar (Maybe (Event -> Maybe (IO ()))) ->
      TVar (Maybe Int) ->
      Maybe V.HTML ->
      st ->
      Int ->
      IO ()
    go conn ctx chan cf oldDom st serverFrame = do
      r <- step ctx st
      case r of
        Nothing -> pure ()
        -- for Left case, we do not update client frame
        Just (NoUpdate _, next, fire) -> do
          atomically $ writeTVar chan (Just fire)
          go conn ctx chan cf oldDom next (serverFrame + 1)
        -- for Right case, we update both the client frame (i.e. sending DOM diff to the websocket)
        -- and server frame
        Just (Update newDom, next, fire) -> do
          clientFrame <- atomically $ do
            a <- readTVar cf
            writeTVar cf Nothing
            pure a

          -- Throw exceptions here
          newDom' <- evaluate newDom

          case oldDom of
            Nothing -> sendTextData conn $ A.encode $ ReplaceDOM newDom'
            Just oldDom' -> do
              -- Throw exceptions here
              diff <- evaluate (V.diff oldDom' newDom')
              sendTextData conn $ A.encode $ UpdateDOM serverFrame clientFrame diff

          atomically $ writeTVar chan (Just fire)

          go conn ctx chan cf (Just newDom) next (serverFrame + 1)
