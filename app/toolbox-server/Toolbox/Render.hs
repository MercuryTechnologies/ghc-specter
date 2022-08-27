{-# LANGUAGE LambdaCase #-}

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
import Control.Lens (to, (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Replica.VDOM.Types (HTML)
import System.IO (IOMode (WriteMode), withFile)
import Toolbox.Channel (Channel (..))
import Toolbox.Render.ModuleGraph (renderModuleGraphTab)
import Toolbox.Render.Session (renderSession)
import Toolbox.Render.Timing (renderTiming)
import Toolbox.Server.Types
  ( Event (..),
    HasServerState (..),
    HasUIState (..),
    ModuleGraphEvent (..),
    ModuleGraphUI (..),
    ServerState (..),
    SubModuleEvent (..),
    Tab (..),
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

renderInbox :: UIState -> Inbox -> Widget HTML Event
renderInbox ui m =
  ul [] $ map eachRender filtered
  where
    tab = ui ^. uiTab
    mexpandedModu = ui ^. uiModuleExpanded
    chan = case tab of
      TabSession -> Session
      TabModuleGraph -> Session
      TabCheckImports -> CheckImports
      TabTiming -> Timing
    filtered = M.toList $ M.filterWithKey (\(c, _) _ -> chan == c) m

    eachRender :: (ChanModule, Text) -> Widget HTML Event
    eachRender ((_, modu), v) =
      let modinfo
            | mexpandedModu == Just modu =
                [ ExpandModuleEv Nothing <$ iconText "fa-minus" modu
                , pre [] [text v]
                ]
            | otherwise =
                [ExpandModuleEv (Just modu) <$ iconText "fa-plus" modu]
       in li [] modinfo

renderMainPanel ::
  UIState ->
  ServerState ->
  Widget HTML Event
renderMainPanel ui ss =
  case ui ^. uiTab of
    TabSession -> renderSession ss
    TabModuleGraph -> renderModuleGraphTab ui ss
    TabCheckImports -> renderInbox ui (ss ^. serverInbox)
    TabTiming -> renderTiming ss

cssLink :: Text -> Widget HTML a
cssLink url =
  el
    "link"
    [ textProp "rel" "stylesheet"
    , textProp "href" url
    ]
    []

renderNavbar :: Tab -> Widget HTML Event
renderNavbar tab =
  nav
    [classList [("navbar m-0 p-0", True)]]
    [ navbarMenu
        [ navbarStart
            [ TabEv TabSession <$ navItem (tab == TabSession) [text "Session"]
            , TabEv TabModuleGraph <$ navItem (tab == TabModuleGraph) [text "Module Graph"]
            , TabEv TabCheckImports <$ navItem (tab == TabCheckImports) [text "CheckImports"]
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

render ::
  (UIState, ServerState) ->
  Widget HTML UIState
render (ui, ss) = do
  let (mainPanel, bottomPanel)
        | ss ^. serverMessageSN == 0 =
            ( div [] [text "No GHC process yet"]
            , divClass "box" [] [text "No Messages"]
            )
        | otherwise =
            ( section
                [style [("height", "85vh"), ("overflow-y", "scroll")]]
                [renderMainPanel ui ss]
            , section
                []
                [divClass "box" [] [text $ "message: " <> (ss ^. serverMessageSN . to (T.pack . show))]]
            )

  let handleNavbar :: Event -> UIState -> UIState
      handleNavbar (TabEv tab') = (uiTab .~ tab')
      handleNavbar _ = id

      handleModuleGraphEv :: ModuleGraphEvent -> ModuleGraphUI -> ModuleGraphUI
      handleModuleGraphEv (HoverOnModuleEv mhovered) mgUI = mgUI {_modGraphUIHover = mhovered}
      handleModuleGraphEv (ClickOnModuleEv mclicked) mgUI = mgUI {_modGraphUIClick = mclicked}

      handleMainPanel :: UIState -> Event -> Widget HTML UIState
      handleMainPanel oldUI (ExpandModuleEv mexpandedModu') = pure oldUI {_uiModuleExpanded = mexpandedModu'}
      handleMainPanel oldUI (MainModuleEv ev) =
        pure oldUI {_uiMainModuleGraph = handleModuleGraphEv ev (_uiMainModuleGraph oldUI)}
      handleMainPanel oldUI (SubModuleEv sev) =
        case sev of
          SubModuleGraphEv ev -> do
            let (d, s) = _uiSubModuleGraph oldUI
                s' = handleModuleGraphEv ev s
            pure oldUI {_uiSubModuleGraph = (d, s')}
          SubModuleLevelEv d' -> do
            let (_, s) = _uiSubModuleGraph oldUI
            pure oldUI {_uiSubModuleGraph = (d', s)}
      handleMainPanel oldUI SaveSessionEv = do
        liftIO $
          withFile "session.json" WriteMode $ \h ->
            BL.hPutStr h (encode ss)
        pure oldUI
      handleMainPanel oldUI _ = pure oldUI

  ui' <-
    div
      [classList [("container is-fullheight is-size-7 m-4 p-4", True)]]
      [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
      , (`handleNavbar` ui) <$> renderNavbar (ui ^. uiTab)
      , handleMainPanel ui =<< mainPanel
      , bottomPanel
      ]
  pure ui'
