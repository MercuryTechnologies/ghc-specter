module GHCSpecter.Render.Session
  ( render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    onClick,
    onInput,
    style,
    textProp,
  )
import Control.Lens ((^.))
import Data.IntMap qualified as IM
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
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
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( button,
    div,
    el,
    input,
    nav,
    p,
    pre,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasUIModel (..),
    UIModel,
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
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
        [ classList [("box", True)]
        , style
            [ ("position", "absolute")
            , ("width", "350px")
            , ("height", "250px")
            , ("top", "0")
            , ("right", "0")
            , ("font-family", "'Lucida Grande',sans-serif")
            , ("font-size", "8px")
            ]
        ]
        (fmap (\x -> p [] [text x]) msgs)

renderConsole ::
  KeyMap DriverId BreakpointLoc ->
  KeyMap DriverId Text ->
  Maybe DriverId ->
  Widget IHTML Event
renderConsole pausedMap consoleMap mcurrConsole = div [] [consoleTabs, console]
  where
    divClass :: Text -> [Props a] -> [Widget IHTML a] -> Widget IHTML a
    divClass cls props = div (classList [(cls, True)] : props)
    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem drvId =
      let isActive = Just drvId == mcurrConsole
          clss
            | isActive = ["navbar-item", "is-tab", "is-active", "m-0", "p-1"]
            | otherwise = ["navbar-item", "is-tab", "m-0", "p-1"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el
            "a"
            [cls, ConsoleEv (ConsoleTab drvId) <$ onClick]
            [text (T.pack (show (unDriverId drvId)))]
    consoleTabs =
      nav
        [classList [("navbar m-0 p-0", True)]]
        [navbarMenu [navbarStart (fmap (navItem . fst) (keyMapToList pausedMap))]]
    consoleContent =
      let mtxt = do
            currConsole <- mcurrConsole
            lookupKey currConsole consoleMap
       in pre
            [ style
                [ ("height", "200px")
                , ("background-color", "black")
                , ("color", "white")
                , ("overflow", "scroll")
                ]
            ]
            [text (fromMaybe "" mtxt)]
    consoleInput =
      divClass
        "control"
        []
        [ input
            [ ConsoleEv ConsoleInput <$ onInput
            , classList [("input", True)]
            , style [("font-family", "monospace")]
            , textProp "type" "text"
            , textProp "placeholder" "type inspection command"
            ]
        ]
    console =
      div [] [consoleContent, consoleInput]

-- | Top-level render function for the Session tab.
render :: UIModel -> ServerState -> Widget IHTML Event
render model ss =
  let sessionInfo = ss ^. serverSessionInfo
      timing = ss ^. serverTiming
      drvModMap = ss ^. serverDriverModuleMap
      pausedMap = ss ^. serverPaused
      consoleMap = ss ^. serverConsole
      mcurrConsole = model ^. modelPausedConsole
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
           in div
                [style [("position", "relative"), ("overflow", "hidden")]]
                ( [ pre [] [text messageTime]
                  , pre [] [text messageProc]
                  , pre [] [text messageModuleStatus]
                  , renderSessionButtons sessionInfo
                  , renderModuleInProgress drvModMap pausedMap timingInProg
                  ]
                    ++ if (sessionIsPaused sessionInfo)
                      then [renderConsole pausedMap consoleMap mcurrConsole]
                      else []
                )
