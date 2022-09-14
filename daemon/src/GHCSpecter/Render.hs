{-# LANGUAGE LambdaCase #-}

module GHCSpecter.Render
  ( ChanModule,
    render,
  )
where

import Concur.Core
  ( SuspendF (..),
    Widget (..),
    unsafeBlockingIO,
  )
import Concur.Replica
  ( Props,
    classList,
    div,
    el,
    nav,
    onClick,
    section,
    style,
    textProp,
  )
import Control.Lens (to, (.~), (^.), _1, _2)
import Control.Monad.Free (liftF)
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
import GHCSpecter.UI.ConcurReplica.DOM (text)
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
  UTCTime ->
  UIState ->
  ServerState ->
  Widget IHTML Event
renderMainPanel stepStartTime ui ss =
  case ui ^. uiTab of
    TabSession -> Session.render ss
    TabModuleGraph -> ModuleGraph.render stepStartTime ui ss
    TabSourceView -> SourceView.render (ui ^. uiSourceView) ss
    TabTiming -> Timing.render ui ss

cssLink :: Text -> Widget IHTML a
cssLink url =
  el
    "link"
    [ textProp "rel" "stylesheet"
    , textProp "href" url
    ]
    []

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
                [renderMainPanel stepStartTime ui ss]
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

  let handleNavbar :: Event -> UIState -> UIState
      handleNavbar (TabEv tab') = (uiTab .~ tab')
      handleNavbar _ = id

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

      handleMainPanel :: (UIState, ServerState) -> Event -> Widget IHTML (UIState, (ServerState, Bool))
      handleMainPanel (oldUI, oldSS) (ExpandModuleEv mexpandedModu') =
        pure ((uiSourceView . srcViewExpandedModule .~ mexpandedModu') oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) (MainModuleEv ev) = do
        let mgui = oldUI ^. uiMainModuleGraph
        (mgui', mxy) <- handleModuleGraphEv ev mgui
        let newUI = (uiMainModuleGraph .~ mgui') oldUI
            newUI' = case mxy of
              Nothing -> newUI
              Just (t, xy) ->
                (uiLastUpdated .~ t) . (uiMousePosition .~ xy) $ newUI
        pure (newUI', (oldSS, False))
      handleMainPanel (oldUI, oldSS) (SubModuleEv sev) =
        case sev of
          SubModuleGraphEv ev -> do
            let mgui = oldUI ^. uiSubModuleGraph . _2
            (mgui', mxy) <- handleModuleGraphEv ev mgui
            let newUI = (uiSubModuleGraph . _2 .~ mgui') oldUI
                newUI' = case mxy of
                  Nothing -> newUI
                  Just (t, xy) ->
                    (uiLastUpdated .~ t) . (uiMousePosition .~ xy) $ newUI
            pure (newUI', (oldSS, False))
          SubModuleLevelEv d' ->
            pure
              ((uiSubModuleGraph . _1 .~ d') oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) (SessionEv SaveSessionEv) = do
        liftIO $
          withFile "session.json" WriteMode $ \h ->
            BL.hPutStr h (encode ss)
        pure (oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) (SessionEv ResumeSessionEv) = do
        let sinfo = oldSS ^. serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = False}
            newSS = (serverSessionInfo .~ sinfo') oldSS
        pure (oldUI, (newSS, True))
      handleMainPanel (oldUI, oldSS) (SessionEv PauseSessionEv) = do
        let sinfo = oldSS ^. serverSessionInfo
            sinfo' = sinfo {sessionIsPaused = True}
        let newSS = (serverSessionInfo .~ sinfo') oldSS
        pure (oldUI, (newSS, True))
      handleMainPanel (oldUI, oldSS) (TimingEv (UpdateSticky b)) =
        pure
          ((uiTiming . timingUISticky .~ b) oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) (TimingEv (UpdatePartition b)) =
        pure
          ((uiTiming . timingUIPartition .~ b) oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) (TimingEv (UpdateParallel b)) =
        pure
          ((uiTiming . timingUIHowParallel .~ b) oldUI, (oldSS, False))
      handleMainPanel (oldUI, oldSS) _ = pure (oldUI, (oldSS, False))

  (ui', (ss', shouldUpdate)) <-
    div
      [classList [("container is-fullheight is-size-7 m-4 p-4", True)]]
      [ cssLink "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
      , cssLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.2/css/all.min.css"
      , (,(ss, False)) <$> (`handleNavbar` ui) <$> renderNavbar (ui ^. uiTab)
      , handleMainPanel (ui, ss) =<< mainPanel
      , bottomPanel
      ]
  pure (ui', (ss', shouldUpdate))
