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
import Toolbox.Server.Types (type ChanModule, type Inbox)
import Prelude hiding (div, span)

divClass :: Text -> [Props a] -> [Widget HTML a] -> Widget HTML a
divClass cls props = div (classList [(cls, True)] : props)

iconText :: Bool -> Text -> Text -> Widget HTML MouseEvent
iconText isClickable ico txt =
  let iconCls = classList [("fas", True), (ico, True)]
      iconProps
        | isClickable = [iconCls, onClick]
        | otherwise = [iconCls]
   in span
        [classList [("icon-text", True)]]
        [ span [classList [("icon", True)]] [el "i" iconProps []]
        , span [] [text txt]
        ]

renderChannel :: Channel -> Maybe Text -> Inbox -> Widget HTML (Maybe Text)
renderChannel chan mexpandedModu m =
  ul [] $ map eachRender filtered
  where
    filtered = M.toList $ M.filterWithKey (\(c, _) _ -> chan == c) m

    eachRender :: (ChanModule, Text) -> Widget HTML (Maybe Text)
    eachRender ((_, modu), v) =
      let modinfo
            | mexpandedModu == Just modu =
                [ iconText False "fa-minus" modu >> pure mexpandedModu
                , pre [] [text v]
                ]
            | otherwise =
                [Just modu <$ iconText True "fa-plus" modu]
       in li [] modinfo

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
            , Trivial <$ navItem (chan == Trivial) [text "Trivial"]
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
  ((Channel, Maybe Text), (Int, Inbox)) ->
  Widget HTML ((Channel, Maybe Text), (Int, Inbox))
render ((chan, mexpandedModu), (i, m)) = do
  let (mainPanel, bottomPanel)
        | i == 0 =
            ( div [] [text "No GHC process yet"]
            , divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [style [("height", "85vh"), ("overflow-y", "scroll")]]
                [renderChannel chan mexpandedModu m]
            , section
                []
                [divClass "box" [] [text $ "message: " <> T.pack (show i)]]
            )
  (chan', mexpandedModu') <-
    div
      [classList [("container is-fullheight", True)]]
      [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
      , ((,mexpandedModu) <$> renderNavbar chan)
      , ((chan,) <$> mainPanel)
      , bottomPanel
      ]
  pure ((chan', mexpandedModu'), (i, m))
