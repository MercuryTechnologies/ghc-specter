module GHCSpecter.Render.Session
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( classList,
    onClick,
    style,
  )
import Control.Lens (to, (^.))
import Data.IntMap qualified as IM
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types
  ( DriverId (..),
    type ModuleName,
  )
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc,
    ModuleGraphInfo (..),
    SessionInfo (..),
    Timer,
    getEndTime,
  )
import GHCSpecter.Render.Util (divClass)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    p,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Constants (widgetHeight)
import GHCSpecter.UI.Types.Event
  ( Event (..),
    SessionEvent (..),
  )
import GHCSpecter.Util.Map
  ( BiKeyMap,
    KeyMap,
    forwardLookup,
    keyMapToList,
    lookupKey,
  )
import Prelude hiding (div)

renderSessionButtons :: SessionInfo -> Widget IHTML Event
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
        , classList [("button", True)]
        ]
        [text "Save Session"]
    buttonPauseResumeSession =
      let (txt, ev)
            | sessionIsPaused session = ("Resume Session", ResumeSessionEv)
            | otherwise = ("Pause Session", PauseSessionEv)
       in button
            [ SessionEv ev <$ onClick
            , classList [("button", True)]
            ]
            [text txt]

renderModuleInProgress ::
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId BreakpointLoc ->
  [(DriverId, Timer)] ->
  Widget IHTML Event
renderModuleInProgress drvModMap pausedMap timingInProg =
  let msgs =
        let is = fmap fst timingInProg
            imodinfos = fmap (\i -> (unDriverId i, forwardLookup i drvModMap, lookupKey i pausedMap)) is
            formatMessage (i, mmod, mpaused) =
              let msgDrvId = T.pack (show i) <> ": "
                  msgModName = fromMaybe "" mmod
                  msgPaused = maybe "" (\loc -> " - paused at " <> T.pack (show loc)) mpaused
               in msgDrvId <> msgModName <> msgPaused
         in fmap formatMessage imodinfos
   in div
        [ classList [("box module-status", True)]
        ]
        (fmap (\x -> p [] [text x]) msgs)

-- | Top-level render function for the Session tab.
render :: ServerState -> Widget IHTML Event
render ss =
  let sessionInfo = ss ^. serverSessionInfo
      timing = ss ^. serverTiming
      drvModMap = ss ^. serverDriverModuleMap
      pausedMap = ss ^. serverPaused
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just sessionStartTime -> do
          let mgi = sessionModuleGraph sessionInfo
              nTot = IM.size (mginfoModuleNameMap mgi)
              timingList = keyMapToList timing
              (timingDone, timingInProg) =
                partition (\(_, t) -> isJust (getEndTime t)) timingList
              nDone = length timingDone
              nInProg = length timingInProg
              messageTime = "Session started at " <> T.pack (show sessionStartTime)
              messageProc = "Session Pid: " <> T.pack (show (sessionProcessId sessionInfo))
              messageModuleStatus =
                "# of modules (done / in progress / total): "
                  <> T.pack (show nDone)
                  <> " / "
                  <> T.pack (show nInProg)
                  <> " / "
                  <> T.pack (show nTot)
           in divClass
                "box"
                [ style
                    [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
                    , ("position", "relative")
                    , ("overflow", "hidden")
                    ]
                ]
                ( [ pre [] [text messageTime]
                  , pre [] [text messageProc]
                  , pre [] [text messageModuleStatus]
                  , renderSessionButtons sessionInfo
                  , renderModuleInProgress drvModMap pausedMap timingInProg
                  ]
                )
