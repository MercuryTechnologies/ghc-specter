{-# OPTIONS_GHC -Werror #-}

module Toolbox.Render.Session
  ( renderSession,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import qualified Data.Map as M
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (SessionInfo (..))
import Toolbox.Server.Types (ServerState (..))
import Prelude hiding (div)

renderSession :: ServerState -> Widget HTML a
renderSession ss =
  let sessionInfo = serverSessionInfo ss
      timing = serverTiming ss
      msg = "# of compiled module now : " <> T.pack (show (M.size timing))
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just sessionStartTime ->
          div
            []
            [ pre [] [text $ T.pack $ show sessionStartTime]
            , pre [] [text msg]
            ]
