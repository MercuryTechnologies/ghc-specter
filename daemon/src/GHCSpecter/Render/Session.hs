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
import Data.Text.Lazy qualified as TL
import GHC.RTS.Flags (RTSFlags)
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
    getEnd,
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
import Text.Pretty.Simple (pShowNoColor)
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

renderCompilationStatus :: (Int, Int, Int) -> Widget IHTML a
renderCompilationStatus (nDone, nInProg, nTot) =
  divClass
    "session-section"
    []
    [text messageModuleStatus]
  where
    messageModuleStatus =
      "# of modules (done / in progress / total): "
        <> T.pack (show nDone)
        <> " / "
        <> T.pack (show nInProg)
        <> " / "
        <> T.pack (show nTot)

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

renderProcessInfo :: ProcessInfo -> Widget IHTML a
renderProcessInfo procinfo =
  divClass
    "session-section"
    []
    [ div [] [spanClass "box" [] [text "Process ID"], text ": ", msgPID]
    , div [] [spanClass "box" [] [text "Executable path"], text ": ", msgPath]
    , div [] [spanClass "box" [] [text "Current Directory"], text ": ", msgCWD]
    , div
        []
        [ div [] [spanClass "box" [] [text "CLI Arguments"], text ":"]
        , div
            [style [("height", "10vh"), ("overflow", "scroll")]]
            msgArgs
        ]
    ]
  where
    msgPID = text $ T.pack $ show $ procPID procinfo
    msgPath = text $ T.pack $ procExecPath procinfo
    msgCWD = text $ T.pack $ procCWD procinfo
    msgArgs =
      let mkItem x = p [style [("margin", "0")]] [text (T.pack x)]
       in fmap mkItem (procArguments procinfo)

renderRTSInfo :: RTSFlags -> Widget IHTML a
renderRTSInfo rtsflags =
  divClass
    "session-section"
    [style [("height", "20vh"), ("overflow", "scroll")]]
    [msg]
  where
    msg = pre [] [text $ TL.toStrict $ pShowNoColor rtsflags]

-- | Top-level render function for the Session tab.
render :: ServerState -> Widget IHTML Event
render ss =
      case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just sessionStartTime -> do
          let messageTime = "Session started at " <> T.pack (show sessionStartTime)
              topPart =
                [ divClass
                    "session-section columns"
                    []
                    [ divClass "column is-one-quarter" [] [text messageTime]
                    , divClass "column is-one-quarter" [] [renderSessionButtons sessionInfo]
                    , divClass "column is-half" [] []
                    ]
                ]
              statusPart =
                [ div
                    []
                    [ divClass "session-title" [] [text "Compilation Status"]
                    , renderCompilationStatus (nDone, nInProg, nTot)
                    ]
                , renderModuleInProgress drvModMap pausedMap timingInProg
                ]
              infoPart =
                flip (maybe []) (sessionProcess sessionInfo) $ \procinfo ->
                  [ div
                      []
                      [ divClass "session-title" [] [text "Process Info"]
                      , renderProcessInfo procinfo
                      ]
                  , div
                      []
                      [ divClass "session-title" [] [text "GHC RTS Info"]
                      , renderRTSInfo (procRTSFlags procinfo)
                      ]
                  ]
           in divClass
                "box"
                [ style
                    [ ("height", ss ^. serverSessionInfo . to sessionIsPaused . to widgetHeight)
                    , ("position", "relative")
                    , ("overflow", "scroll")
                    ]
                ]
                (topPart ++ statusPart ++ infoPart)
  where
      sessionInfo = ss ^. serverSessionInfo
      mgi = sessionModuleGraph sessionInfo
      timing = ss ^. serverTiming . tsTimingMap
      drvModMap = ss ^. serverDriverModuleMap
      pausedMap = ss ^. serverPaused
      nTot = IM.size (mginfoModuleNameMap mgi)
      timingList = keyMapToList timing
      (timingDone, timingInProg) =
        partition (\(_, t) -> isJust (getEnd t)) timingList
      nDone = length timingDone
      nInProg = length timingInProg
