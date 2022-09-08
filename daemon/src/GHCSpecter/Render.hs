{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Render
  ( ChanModule,
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
    section,
    style,
    text,
    textProp,
  )
import Control.Lens (to, (%~), (.~), (^.), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import GHCSpecter.Render.ModuleGraph qualified as ModuleGraph
import GHCSpecter.Render.Session qualified as Session
import GHCSpecter.Render.SourceView qualified as SourceView
import GHCSpecter.Render.Timing qualified as Timing
import GHCSpecter.Server.Types
  ( Event (..),
    HasModuleGraphUI (..),
    HasServerState (..),
    HasSourceViewUI (..),
    HasUIState (..),
    ModuleGraphEvent (..),
    ModuleGraphUI (..),
    ServerState (..),
    SubModuleEvent (..),
    Tab (..),
    UIState (..),
    type ChanModule,
  )
import Replica.VDOM.Types (HTML)
import System.IO (IOMode (WriteMode), withFile)
import Prelude hiding (div, span)

divClass :: Text -> [Props a] -> [Widget HTML a] -> Widget HTML a
divClass cls props = div (classList [(cls, True)] : props)

renderMainPanel ::
  UIState ->
  ServerState ->
  Widget HTML Event
renderMainPanel ui ss =
  case ui ^. uiTab of
    TabSession -> Session.render ss
    TabModuleGraph -> ModuleGraph.render ui ss
    TabSourceView -> SourceView.render (ui ^. uiSourceView) ss
    TabTiming -> Timing.render ui ss

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
      handleModuleGraphEv (HoverOnModuleEv mhovered) = modGraphUIHover .~ mhovered
      handleModuleGraphEv (ClickOnModuleEv mclicked) = modGraphUIClick .~ mclicked

      handleMainPanel :: UIState -> Event -> Widget HTML UIState
      handleMainPanel oldUI (ExpandModuleEv mexpandedModu') =
        pure $ (uiSourceView . srcViewExpandedModule .~ mexpandedModu') oldUI
      handleMainPanel oldUI (MainModuleEv ev) =
        pure $ (uiMainModuleGraph %~ handleModuleGraphEv ev) oldUI
      handleMainPanel oldUI (SubModuleEv sev) =
        case sev of
          SubModuleGraphEv ev ->
            pure $ (uiSubModuleGraph . _2 %~ handleModuleGraphEv ev) oldUI
          SubModuleLevelEv d' ->
            pure $ (uiSubModuleGraph . _1 .~ d') oldUI
      handleMainPanel oldUI SaveSessionEv = do
        liftIO $
          withFile "session.json" WriteMode $ \h ->
            BL.hPutStr h (encode ss)
        pure oldUI
      handleMainPanel oldUI (TimingPanelEv b) = do
        liftIO $
          putStrLn $ "TimingPanelEv:" ++ show b
        pure $ (uiTimingSticky .~ b) oldUI
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
