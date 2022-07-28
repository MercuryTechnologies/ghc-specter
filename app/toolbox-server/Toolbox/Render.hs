module Toolbox.Render
  ( ChanModule,
    Inbox,
    render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( MouseEvent,
    Props,
    classList,
    div,
    el,
    li,
    nav,
    onClick,
    pre,
    section,
    span,
    style,
    text,
    textProp,
    ul,
  )
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (Channel (..))
import Toolbox.Render.Timing
  ( renderTiming,
    renderTimingChart,
  )
import Toolbox.Server.Types
  ( ServerState (..),
    UIState (..),
    type ChanModule,
    type Inbox,
  )
import Prelude hiding (div, span)

divClass :: Text -> [Props a] -> [Widget HTML a] -> Widget HTML a
divClass cls props = div (classList [(cls, True)] : props)

iconText :: Text -> Text -> Widget HTML MouseEvent
iconText ico txt =
  let iconCls = classList [("fas", True), (ico, True)]
      iconProps = [iconCls, onClick]
   in span
        [classList [("icon-text", True)]]
        [ span [classList [("icon", True)]] [el "i" iconProps []]
        , span [onClick] [text txt]
        ]

renderInbox :: (Channel, Maybe Text) -> Inbox -> Widget HTML (Maybe Text)
renderInbox (chan, mexpandedModu) m =
  ul [] $ map eachRender filtered
  where
    filtered = M.toList $ M.filterWithKey (\(c, _) _ -> chan == c) m

    eachRender :: (ChanModule, Text) -> Widget HTML (Maybe Text)
    eachRender ((_, modu), v) =
      let modinfo
            | mexpandedModu == Just modu =
                [ Nothing <$ iconText "fa-minus" modu
                , pre [] [text v]
                ]
            | otherwise =
                [Just modu <$ iconText "fa-plus" modu]
       in li [] modinfo

renderMainPanel ::
  UIState ->
  ServerState ->
  Widget HTML (Maybe Text)
renderMainPanel (UIState chan mexpandedModu) ss =
  case chan of
    CheckImports -> renderInbox (CheckImports, mexpandedModu) (serverInbox ss)
    Timing ->
      div
        []
        [ pre [] [text $ T.pack $ show (serverSessionInfo ss)]
        , div [] [renderTiming ss]
        -- , renderTiming ss -- renderInbox (Timing, mexpandedModu) (serverInbox ss)
        ]
    _ -> renderInbox (Timing, mexpandedModu) (serverInbox ss)

cssLink :: Text -> Widget HTML a
cssLink url =
  el
    "link"
    [ textProp "rel" "stylesheet"
    , textProp "href" url
    ]
    []

renderNavbar :: Channel -> Widget HTML Channel
renderNavbar chan =
  nav
    [classList [("navbar", True)]]
    [ navbarMenu
        [ navbarStart
            [ CheckImports <$ navItem (chan == CheckImports) [text "CheckImports"]
            , Timing <$ navItem (chan == Timing) [text "Timing"]
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

render ::
  (UIState, ServerState) ->
  Widget HTML UIState
render (ui@(UIState chan mexpandedModu), ss) = do
  let (mainPanel, bottomPanel)
        | serverMessageSN ss == 0 =
            ( div [] [text "No GHC process yet"]
            , divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [style [("height", "85vh"), ("overflow-y", "scroll")]]
                [renderMainPanel ui ss]
            , section
                []
                [divClass "box" [] [text $ "message: " <> T.pack (show (serverMessageSN ss))]]
            )
  ui' <-
    div
      [classList [("container is-fullheight", True)]]
      [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
      , (\chan' -> UIState chan' mexpandedModu) <$> renderNavbar chan
      , (\mexpandedModu' -> UIState chan mexpandedModu') <$> mainPanel
      , bottomPanel
      ]
  pure ui'
