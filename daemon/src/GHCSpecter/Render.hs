{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Render
  ( ChanModule,
    render,
  )
where

import Concur.Core
  ( Widget (..),
    unsafeBlockingIO,
  )
import Concur.Replica
  ( Props,
    classList,
    onClick,
    style,
    textProp,
  )
import Control.Lens (to, (.~), (^.), _1, _2)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHCSpecter.Channel (SessionInfo (..))
import GHCSpecter.Render.ModuleGraph qualified as ModuleGraph
import GHCSpecter.Render.Session qualified as Session
import GHCSpecter.Render.SourceView qualified as SourceView
import GHCSpecter.Render.Timing qualified as Timing
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
    type ChanModule,
  )
import GHCSpecter.UI.ConcurReplica.DOM
  ( div,
    el,
    link,
    nav,
    section,
    text,
  )
import GHCSpecter.UI.ConcurReplica.Types (IHTML)
import GHCSpecter.UI.Types
  ( HasModuleGraphUI (..),
    HasSourceViewUI (..),
    HasTimingUI (..),
    HasUIState (..),
    ModuleGraphUI (..),
    UIState (..),
  )
import GHCSpecter.UI.Types.Event
  ( Event (..),
    ModuleGraphEvent (..),
    SessionEvent (..),
    SubModuleEvent (..),
    Tab (..),
    TimingEvent (..),
  )
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (div, span)

divClass :: Text -> [Props a] -> [Widget IHTML a] -> Widget IHTML a
divClass cls props = div (classList [(cls, True)] : props)

renderMainPanel ::
  UIState ->
  ServerState ->
  Widget IHTML Event
renderMainPanel ui ss =
  case ui ^. uiTab of
    TabSession -> Session.render ss
    TabModuleGraph -> ModuleGraph.render ui ss
    TabSourceView -> SourceView.render (ui ^. uiSourceView) ss
    TabTiming -> Timing.render ui ss

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

tempRef :: IORef Int
tempRef = unsafePerformIO (newIORef 0)
{-# NOINLINE tempRef #-}

handleEvent :: Event -> UTCTime -> (UIState, ServerState) -> Widget IHTML (UIState, (ServerState, Bool))
handleEvent topEv stepStartTime (oldUI, oldSS) =
  case topEv of
    TabEv tab' ->
      pure ((uiTab .~ tab') oldUI, (oldSS, False))
    ExpandModuleEv mexpandedModu' ->
      pure ((uiSourceView . srcViewExpandedModule .~ mexpandedModu') oldUI, (oldSS, False))
    MainModuleEv ev -> do
      let mgui = oldUI ^. uiMainModuleGraph
      (mgui', mxy) <- handleModuleGraphEv ev mgui
      let newUI = (uiMainModuleGraph .~ mgui') oldUI
          newUI' = handleMouseMove newUI mxy
      pure (newUI', (oldSS, False))
    SubModuleEv sev ->
      case sev of
        SubModuleGraphEv ev -> do
          let mgui = oldUI ^. uiSubModuleGraph . _2
          (mgui', mxy) <- handleModuleGraphEv ev mgui
          let newUI = (uiSubModuleGraph . _2 .~ mgui') oldUI
              newUI' = handleMouseMove newUI mxy
          pure (newUI', (oldSS, False))
        SubModuleLevelEv d' ->
          pure
            ((uiSubModuleGraph . _1 .~ d') oldUI, (oldSS, False))
    SessionEv SaveSessionEv -> do
      -- TODO: use asynchronous worker
      liftIO $
        withFile "session.json" WriteMode $ \h ->
          BL.hPutStr h (encode oldSS)
      pure (oldUI, (oldSS, False))
    SessionEv ResumeSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = False}
          newSS = (serverSessionInfo .~ sinfo') oldSS
      pure (oldUI, (newSS, True))
    SessionEv PauseSessionEv -> do
      let sinfo = oldSS ^. serverSessionInfo
          sinfo' = sinfo {sessionIsPaused = True}
          newSS = (serverSessionInfo .~ sinfo') oldSS
      pure (oldUI, (newSS, True))
    TimingEv (UpdateSticky b) ->
      pure
        ((uiTiming . timingUISticky .~ b) oldUI, (oldSS, False))
    TimingEv (UpdatePartition b) ->
      pure
        ((uiTiming . timingUIPartition .~ b) oldUI, (oldSS, False))
    TimingEv (UpdateParallel b) ->
      pure
        ((uiTiming . timingUIHowParallel .~ b) oldUI, (oldSS, False))
  where
    handleModuleGraphEv ::
      ModuleGraphEvent ->
      ModuleGraphUI ->
      Widget IHTML (ModuleGraphUI, Maybe (UTCTime, (Double, Double)))
    handleModuleGraphEv (HoverOnModuleEv mhovered) mgui =
      pure ((modGraphUIHover .~ mhovered) mgui, Nothing)
    handleModuleGraphEv (ClickOnModuleEv mclicked) mgui =
      pure ((modGraphUIClick .~ mclicked) mgui, Nothing)
    handleModuleGraphEv (DummyEv mxy) mgui = do
      t <-
        unsafeBlockingIO $ do
          n <- readIORef tempRef
          modifyIORef' tempRef (+ 1)
          t <- getCurrentTime
          print (n, stepStartTime, t, mxy)
          pure t
      pure (mgui, (t,) <$> mxy)

    handleMouseMove ui_ mxy =
      case mxy of
        Nothing -> ui_
        Just (t, xy) ->
          if ui_ ^. uiShouldUpdate
            then (uiLastUpdated .~ t) . (uiMousePosition .~ xy) $ ui_
            else uiMousePosition .~ xy $ ui_

render ::
  UTCTime ->
  (UIState, ServerState) ->
  Widget IHTML (UIState, (ServerState, Bool))
render stepStartTime (ui, ss) = do
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
                [ divClass
                    "box"
                    []
                    [ text $ "message: " <> (ss ^. serverMessageSN . to (T.pack . show))
                    , text $ "(x,y): " <> (ui ^. uiMousePosition . to (T.pack . show))
                    ]
                ]
            )
  ev <-
    div
      [classList [("container is-fullheight is-size-7 m-4 p-4", True)]]
      [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
      , renderNavbar (ui ^. uiTab)
      , mainPanel
      , bottomPanel
      ]
  (ui', (ss', shouldUpdate)) <- handleEvent ev stepStartTime (ui, ss)
  pure (ui', (ss', shouldUpdate))
