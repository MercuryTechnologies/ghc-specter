{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Render
  ( render,
  )
where

import Concur.Core (Widget (..))
import Concur.Replica
  ( Props,
    classList,
    onClick,
    src,
    style,
    textProp,
  )
import Control.Lens (to, (^.))
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Data.Assets (HasAssets (..))
import GHCSpecter.Render.ModuleGraph qualified as ModuleGraph
import GHCSpecter.Render.Session qualified as Session
import GHCSpecter.Render.SourceView qualified as SourceView
import GHCSpecter.Render.Timing qualified as Timing
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    figure,
    img,
    link,
    nav,
    progress,
    section,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasMainView (..),
    HasUIModel (..),
    HasUIState (..),
    MainView,
    UIModel (..),
    UIState (..),
    UIView (..),
  )
import GHCSpecter.UI.Types.Event
  ( Event (..),
    Tab (..),
  )
import Prelude hiding (div, span)

divClass :: Text -> [Props a] -> [Widget IHTML a] -> Widget IHTML a
divClass cls props = div (classList [(cls, True)] : props)

renderMainPanel ::
  MainView ->
  UIModel ->
  ServerState ->
  Widget IHTML Event
renderMainPanel view model ss =
  case view ^. mainTab of
    TabSession -> Session.render ss
    TabModuleGraph -> ModuleGraph.render model ss
    TabSourceView -> SourceView.render (model ^. modelSourceView) ss
    TabTiming -> Timing.render model ss

cssLink :: Text -> Widget IHTML a
cssLink url =
  link
    [ textProp "rel" "stylesheet"
    , textProp "href" url
    ]

renderNavbar :: Tab -> Widget IHTML Event
renderNavbar tab =
  nav
    [classList [("navbar m-0 p-0", True)]]
    [ navbarMenu
        [ navbarStart
            [ TabEv TabSession <$ navItem (tab == TabSession) [text "Session"]
            , TabEv TabModuleGraph <$ navItem (tab == TabModuleGraph) [text "Module Graph"]
            , TabEv TabSourceView <$ navItem (tab == TabSourceView) [text "Source View"]
            , TabEv TabTiming <$ navItem (tab == TabTiming) [text "Timing"]
            ]
        ]
    ]
  where
    navbarMenu = divClass "navbar-menu" []
    navbarStart = divClass "navbar-start" []
    navItem isActive =
      let clss
            | isActive = ["navbar-item", "is-tab", "is-active", "m-0", "p-1"]
            | otherwise = ["navbar-item", "is-tab", "m-0", "p-1"]
          cls = classList $ map (\tag -> (tag, True)) clss
       in el "a" [cls, onClick]

renderMainView :: (MainView, UIModel, ServerState) -> Widget IHTML Event
renderMainView (view, model, ss) = do
  let (mainPanel, bottomPanel)
        | ss ^. serverMessageSN == 0 =
            ( div [] [text "No GHC process yet"]
            , divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [style [("height", "85vh"), ("overflow-y", "scroll")]]
                [renderMainPanel view model ss]
            , section
                []
                [ divClass
                    "box"
                    []
                    [ text $ "message: " <> (ss ^. serverMessageSN . to (T.pack . show))
                    , text $ "(x,y): " <> (model ^. modelMousePosition . to (T.pack . show))
                    ]
                ]
            )
  div
    [classList [("container is-fullheight is-size-7 m-4 p-4", True)]]
    [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
    , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
    , renderNavbar (view ^. mainTab)
    , mainPanel
    -- , bottomPanel
    ]

render ::
  (UIState, ServerState) ->
  Widget IHTML Event
render (ui, ss) =
  case ui ^. uiView of
    BannerMode v ->
      div
        [classList [("container is-fullheight is-size-7 m-4 p-4", True)]]
        [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
        , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
        , section
            [classList [("hero is-medium is-link is-size-1 m-1 p-1", True)]]
            [ div
                [classList [("hero-body columns", True)]]
                [ figure
                    [classList [("column image", True)]]
                    [img [src (ui ^. uiAssets . assetsGhcSpecterPng)]]
                , div
                    [classList [("column has-text-centered", True)]]
                    [ text "ghc-specter"
                    , progress
                        [ classList [("progress is-primary", True)]
                        , textProp "value" (T.pack $ show $ floor @_ @Int (v * 100.0))
                        , textProp "max" "100"
                        ]
                        []
                    ]
                ]
            ]
        ]
    MainMode view -> renderMainView (view, ui ^. uiModel, ss)
