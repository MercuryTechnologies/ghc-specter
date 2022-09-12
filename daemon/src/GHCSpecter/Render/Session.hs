module GHCSpecter.Render.Session
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( button,
    classList,
    div,
    onClick,
    pre,
    text,
  )
import Control.Lens ((^.))
import Data.Map qualified as M
import Data.Text qualified as T
import GHCSpecter.Channel (SessionInfo (..))
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.Types.Event
  ( Event (..),
    SessionEvent (..),
  )
import Replica.VDOM.Types (HTML)
import Prelude hiding (div)

renderSessionButtons :: SessionInfo -> Widget HTML Event
renderSessionButtons session =
  div
    []
    [ buttonSaveSession
    , buttonPauseResumeSession
    ]
  where
    buttonSaveSession =
      button
        [ SessionEv SaveSessionEv <$ onClick
        , classList [("button is-primary is-size-7 m-1 p-1", True)]
        ]
        [text "Save Session"]
    buttonPauseResumeSession =
      let (txt, ev)
            | sessionIsPaused session = ("Resume Session", ResumeSessionEv)
            | otherwise = ("Pause Session", PauseSessionEv)
       in button
            [ SessionEv ev <$ onClick
            , classList [("button is-primary is-size-7 m-1 p-1", True)]
            ]
            [text txt]

-- | Top-level render function for the Session tab.
render :: ServerState -> Widget HTML Event
render ss =
  let sessionInfo = ss ^. serverSessionInfo
      timing = ss ^. serverTiming
      msg = "# of compiled module now : " <> T.pack (show (M.size timing))
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just sessionStartTime ->
          div
            []
            [ pre [] [text $ T.pack $ show sessionStartTime]
            , pre [] [text msg]
            , renderSessionButtons sessionInfo
            ]
