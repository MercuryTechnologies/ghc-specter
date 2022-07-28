{-# OPTIONS_GHC -Werror #-}

module Toolbox.Render.Session
  ( renderSession,
  )
where

import Concur.Core (Widget)
import Concur.Replica (pre, text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (SessionInfo (..))
import Toolbox.Server.Types (ServerState (..))

renderSession :: ServerState -> Widget HTML a
renderSession ss =
  case sessionStartTime (serverSessionInfo ss) of
    Nothing -> pre [] [text "GHC Session has not been started"]
    Just sessionStartTime ->
      pre [] [text $ T.pack $ show sessionStartTime]
