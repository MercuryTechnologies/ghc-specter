module Toolbox.Render
  ( ChanModule,
    Inbox,
    render,
  )
where

import Concur.Core (Widget)
import Concur.Replica
  ( Props,
    classList,
    div,
    el,
    nav,
    onClick,
    pre,
    section,
    style,
    text,
    textProp,
  )
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel (Channel (..))
import Toolbox.Server.Types (type ChanModule, type Inbox)
import Prelude hiding (div)

divClass :: Text -> [Props a] -> [Widget HTML a] -> Widget HTML a
divClass cls props = div (classList [(cls, True)] : props)

renderChannel :: Channel -> Inbox -> Widget HTML a
renderChannel chan m =
  div [] $ map eachRender filtered
  where
    filtered = M.toList $ M.filterWithKey (\(c, _) _ -> chan == c) m

    eachRender :: (ChanModule, Text) -> Widget HTML a
    eachRender ((_, modu), v) =
      div
        []
        [ text ("chan: " <> T.pack (show chan) <> ", module: " <> modu)
        , pre [] [text v]
        , pre [] [text "-----------"]
        ]

cssLink :: Widget HTML a
cssLink =
  el
    "link"
    [ textProp "rel" "stylesheet"
    , textProp "href" "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
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
    navItem b =
      let cls =
            classList $
              map (\tag -> (tag, True)) (if b then ["navbar-item", "is-tab", "is-active"] else ["navbar-item", "is-tab"])
       in el "a" [cls, onClick]

render ::
  (Channel, (Int, Inbox)) ->
  Widget HTML (Channel, (Int, Inbox))
render (chan, (i, m)) = do
  let (mainPanel, bottomPanel)
        | i == 0 =
            ( div [] [text "No GHC process yet"]
            , divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [style [("height", "85vh"), ("overflow-y", "scroll")]]
                [renderChannel chan m]
            , section
                []
                [divClass "box" [] [text $ "message: " <> T.pack (show i)]]
            )
  chan' <-
    div
      [classList [("container is-fullheight", True)]]
      [ cssLink
      , renderNavbar chan
      , mainPanel
      , bottomPanel
      ]
  pure (chan', (i, m))
