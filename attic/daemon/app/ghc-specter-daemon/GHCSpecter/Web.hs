{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Web
  ( render,
  )
where

import Concur.Core (Widget (..))
import Concur.Replica
  ( classList,
    height,
    onClick,
    src,
    style,
    textProp,
    width,
  )
import Control.Lens (to, (^.))
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Channel.Common.Types (DriverId (..))
import GHCSpecter.Channel.Outbound.Types (SessionInfo (..))
import GHCSpecter.ConcurReplica.DOM
  ( div,
    el,
    img,
    nav,
    p,
    progress,
    section,
    text,
  )
import GHCSpecter.ConcurReplica.Types (IHTML)
import GHCSpecter.Data.Assets (HasAssets (..))
import GHCSpecter.Data.Map (forwardLookup, keyMapToList, lookupKey)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.Help (consoleCommandList)
import GHCSpecter.UI.Types
  ( HasConsoleUI (..),
    HasUIModel (..),
    HasUIState (..),
    UIModel (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( ConsoleEvent (..),
    Event (..),
    Tab (..),
    UserEvent (..),
  )
import GHCSpecter.Web.Console qualified as Console
import GHCSpecter.Web.ModuleGraph qualified as ModuleGraph
import GHCSpecter.Web.Session qualified as Session
import GHCSpecter.Web.SourceView qualified as SourceView
import GHCSpecter.Web.Timing qualified as Timing
import GHCSpecter.Web.Util (divClass)
import Prelude hiding (div, span)

renderBanner :: Text -> Double -> Widget IHTML a
renderBanner png fraction = divClass "banner" [] [contents]
  where
    progressBar =
      progress
        [ textProp "value" (T.pack $ show $ floor @_ @Int (fraction * 100.0)),
          textProp "max" "100"
        ]
        []
    contents =
      div
        []
        [ img [src png, width "150px", height "150px"],
          div [] [p [] [text "ghc-specter"], p [] [progressBar]]
        ]

renderNavbar :: Tab -> Widget IHTML Event
renderNavbar tab =
  nav
    [classList [("navbar", True)]]
    [ navbarMenu
        [ navbarStart
            [ UsrEv (TabEv TabSession) <$ navItem (tab == TabSession) [text "Session"],
              UsrEv (TabEv TabModuleGraph) <$ navItem (tab == TabModuleGraph) [text "Module Graph"],
              UsrEv (TabEv TabSourceView) <$ navItem (tab == TabSourceView) [text "Source View"],
              UsrEv (TabEv TabTiming) <$ navItem (tab == TabTiming) [text "Timing"]
            ]
        ]
    ]
  where
    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem isActive =
      let clss
            | isActive = ["navbar-item", "is-tab", "is-active"]
            | otherwise = ["navbar-item", "is-tab"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el "a" [cls, onClick]

renderMainPanel ::
  UIModel ->
  ServerState ->
  Widget IHTML Event
renderMainPanel model ss =
  case model ^. modelTab of
    TabSession -> Session.render ss
    TabModuleGraph -> ModuleGraph.render model ss
    TabSourceView -> SourceView.render (model ^. modelSourceView) ss
    TabTiming -> Timing.render model ss

renderBottomPanel :: UIModel -> ServerState -> Widget IHTML Event
renderBottomPanel model ss = div [] (consolePanel ++ [msgCounter])
  where
    sessionInfo = ss ^. serverSessionInfo
    pausedMap = ss ^. serverPaused
    consoleMap = ss ^. serverConsole
    mconsoleFocus = model ^. modelConsole . consoleFocus
    inputEntry = model ^. modelConsole . consoleInputEntry
    -- TODO: refactor these out
    getTabName k =
      let ktxt = T.pack $ show (unDriverId k)
          mlookedup = forwardLookup k (ss ^. serverDriverModuleMap)
       in maybe ktxt (\m -> ktxt <> " - " <> m) mlookedup
    getHelp k =
      let title =
            let mpaused = lookupKey k pausedMap
             in maybe "" (\loc -> "paused at " <> T.pack (show loc)) mpaused
          classify txt =
            if txt == ":next" || txt == ":goto-source" || txt == ":dump-heap" || txt == ":exit-ghc-debug" || txt == ":list-core"
              then Left (txt, ConsoleButtonPressed True txt)
              else Right txt
          helpMsgs =
            maybe
              [Right "No Help!"]
              (fmap classify)
              (consoleCommandList <$> lookupKey k (ss ^. serverPaused))
       in (title, helpMsgs)
    consolePanel
      | sessionIsPaused sessionInfo =
          [ UsrEv . ConsoleEv
              <$> Console.render
                (fmap (\(k, _) -> (k, getTabName k)) . keyMapToList $ pausedMap)
                consoleMap
                getHelp
                mconsoleFocus
                inputEntry
          ]
      | otherwise = []

    msgCounter =
      divClass "box" [] [text $ "message: " <> (ss ^. serverMessageSN . to (T.pack . show))]

renderMainView :: (UIModel, ServerState) -> Widget IHTML Event
renderMainView (model, ss) = do
  let (mainPanel, bottomPanel)
        | ss ^. serverMessageSN == 0 =
            ( div [] [text "No GHC process yet"],
              divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [ style
                    [ ("max-height", "95vh"),
                      ("overflow", "hidden")
                    ]
                ]
                [renderMainPanel model ss],
              section [] [renderBottomPanel model ss]
            )
  div
    [ classList [("is-fullheight", True)],
      style [("overflow", "hidden")]
    ]
    [ renderNavbar (model ^. modelTab),
      mainPanel,
      bottomPanel
    ]

render ::
  (UIState, ServerState) ->
  Widget IHTML Event
render (ui, ss) =
  case ui ^. uiModel . modelTransientBanner of
    Just fraction -> renderBanner (ui ^. uiAssets . assetsGhcSpecterPng) fraction
    Nothing -> renderMainView (ui ^. uiModel, ss)
