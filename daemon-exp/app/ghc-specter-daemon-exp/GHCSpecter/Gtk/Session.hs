{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Gtk.Session (renderSession) where

import Control.Lens (to, (^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (for_)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import GHCSpecter.Channel.Outbound.Types (
  getEnd,
 )
import GHCSpecter.Data.Map (keyMapToList)
import GHCSpecter.Graphics.DSL (Color (..), Scene (..), ViewPort (..))
import GHCSpecter.Gtk.Renderer (
  render,
  setColor,
 )
import GHCSpecter.Gtk.Types (GtkRender, ViewBackend (..))
import GHCSpecter.Gtk.Util.Rules (boxRules)
import GHCSpecter.Server.Types (
  HasServerState (..),
  HasTimingState (..),
  ServerState,
 )
import GHCSpecter.UI.Constants (HasWidgetConfig (..))
import GHCSpecter.UI.Session (
  buildModuleInProgress,
  buildPauseResume,
  buildSession,
 )
import GHCSpecter.UI.Types (
  HasSessionUI (..),
  HasViewPortInfo (..),
  SessionUI,
 )
import GHCSpecter.UI.Types.Event (Event (..))
import GHCSpecter.Util.Transformation (translateToOrigin)
import GI.Cairo.Render qualified as R

renderSession ::
  ServerState ->
  SessionUI ->
  GtkRender Event ()
renderSession ss sessui = do
  wcfg <- (^. to vbWidgetConfig . wcfgSession) <$> ask
  for_ (Map.lookup "session-main" wcfg) $ \vpCvs -> do
    let vpiMain = sessui ^. sessionUIMainViewPort
        vpMain = fromMaybe (vpiMain ^. vpViewPort) (vpiMain ^. vpTempViewPort)
    sceneMain <- buildSession ss
    let sceneMain' =
          sceneMain
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vpMain
            }
    render sceneMain'
  for_ (Map.lookup "module-status" wcfg) $ \vpCvs -> do
    let ViewPort (cx0, cy0) (cx1, cy1) = vpCvs
    setColor Ivory
    -- TODO: this should be wrapped in a function.
    lift $ do
      R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
      R.fill
    boxRules vpCvs

    let vpiStatus = sessui ^. sessionUIModStatusViewPort
        vpStatus = fromMaybe (vpiStatus ^. vpViewPort) (vpiStatus ^. vpTempViewPort)
        drvModMap = ss ^. serverDriverModuleMap
        timing = ss ^. serverTiming . tsTimingMap
        pausedMap = ss ^. serverPaused
        timingList = keyMapToList timing
        (_timingDone, timingInProg) =
          partition (\(_, t) -> isJust (getEnd t)) timingList
    sceneModStatus <- buildModuleInProgress drvModMap pausedMap timingInProg
    let sceneModStatus' =
          sceneModStatus
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = vpStatus
            }
    render sceneModStatus'
  for_ (Map.lookup "session-button" wcfg) $ \vpCvs -> do
    let sessionInfo = ss ^. serverSessionInfo
    scenePause <- fmap (fmap SessionEv) <$> buildPauseResume sessionInfo
    let scenePause' =
          scenePause
            { sceneGlobalViewPort = vpCvs
            , sceneLocalViewPort = translateToOrigin vpCvs
            }
    render scenePause'
