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
    ProcessInfo (..),
    SessionInfo (..),
    Timer,
    getEndTime,
  )
import GHCSpecter.Data.Map
  ( BiKeyMap,
    KeyMap,
    forwardLookup,
    keyMapToList,
    lookupKey,
  )
import GHCSpecter.Render.Util (divClass, spanClass)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    HasTimingState (..),
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

renderProcessInfo :: ProcessInfo -> Widget IHTML Event
renderProcessInfo procinfo =
  divClass
    "process-info"
    []
    [ div [] [spanClass "box" [] [text "Process ID"], text ": ", msgPID]
    , div [] [spanClass "box" [] [text "Executable path"], text ": ", msgPath]
    , div [] [spanClass "box" [] [text "Current Directory"], text ": ", msgCWD]
    , div [] [spanClass "box" [] [text "CLI Arguments"], text ": ", msgArgs]
    ]
  where
    packShow = T.pack . show
    msgPID = text $ packShow $ procPID procinfo
    msgPath = text $ T.pack $ procExecPath procinfo
    msgCWD = text $ T.pack $ procCWD procinfo
    msgArgs = text $ T.intercalate " " (fmap T.pack (procArguments procinfo))

-- | Top-level render function for the Session tab.
render :: ServerState -> Widget IHTML Event
render ss =
  let sessionInfo = ss ^. serverSessionInfo
      timing = ss ^. serverTiming . tsTimingMap
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
                  , renderProcessInfo (sessionProcess sessionInfo)
                  , pre [] [text messageModuleStatus]
                  , renderSessionButtons sessionInfo
                  , renderModuleInProgress drvModMap pausedMap timingInProg
                  ]
                )
