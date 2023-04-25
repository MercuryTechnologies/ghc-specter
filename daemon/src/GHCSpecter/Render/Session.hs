{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.Render.Session (
  buildModuleInProgress,
  buildSession,
  buildPauseResume,
) where

import Control.Lens ((^.))
import Data.IntMap qualified as IM
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHCSpecter.Channel.Common.Types (
  DriverId (..),
  ModuleName,
 )
import GHCSpecter.Channel.Outbound.Types (
  BreakpointLoc,
  ModuleGraphInfo (..),
  ProcessInfo (..),
  SessionInfo (..),
  Timer,
  getEnd,
 )
import GHCSpecter.Data.Map (
  BiKeyMap,
  KeyMap,
  forwardLookup,
  keyMapToList,
  lookupKey,
 )
import GHCSpecter.Graphics.DSL (
  Color (..),
  HitEvent (..),
  Scene (..),
  TextFontFace (Sans),
  TextPosition (..),
  ViewPort (..),
  drawText,
  rectangle,
 )
import GHCSpecter.Render.Components.TextView (buildTextView)
import GHCSpecter.Server.Types (
  HasModuleGraphState (..),
  HasServerState (..),
  HasTimingState (..),
  ServerState (..),
 )
import GHCSpecter.UI.Types.Event (
  SessionEvent (..),
 )
import Text.Pretty.Simple (pShowNoColor)
import Prelude hiding (div)

buildModuleInProgress ::
  BiKeyMap DriverId ModuleName ->
  KeyMap DriverId BreakpointLoc ->
  [(DriverId, Timer)] ->
  Scene e
buildModuleInProgress drvModMap pausedMap timingInProg =
  scene {sceneId = "module-status"}
  where
    msgs =
      let is = fmap fst timingInProg
          imodinfos = fmap (\i -> (unDriverId i, forwardLookup i drvModMap, lookupKey i pausedMap)) is
          formatMessage (i, mmod, mpaused) =
            let msgDrvId = T.pack (show i) <> ": "
                msgModName = fromMaybe "" mmod
                msgPaused = maybe "" (\loc -> " - paused at " <> T.pack (show loc)) mpaused
             in msgDrvId <> msgModName <> msgPaused
       in fmap formatMessage imodinfos
    scene = buildTextView (T.unlines msgs) []

buildSession ::
  ServerState ->
  Scene e
buildSession ss =
  scene {sceneId = "session-main"}
  where
    sessionInfo = ss ^. serverSessionInfo
    mgi = ss ^. serverModuleGraphState . mgsModuleGraphInfo
    timing = ss ^. serverTiming . tsTimingMap
    timingList = keyMapToList timing
    (timingDone, timingInProg) =
      partition (\(_, t) -> isJust (getEnd t)) timingList
    nTot = IM.size (mginfoModuleNameMap mgi)
    nDone = length timingDone
    nInProg = length timingInProg

    msgSessionStart =
      case sessionStartTime sessionInfo of
        Nothing ->
          "GHC Session has not been started"
        Just sessionStartTime ->
          "Session started at " <> T.pack (show sessionStartTime)
    msgCompilationStatus =
      "# of modules (done / in progress / total): "
        <> T.pack (show nDone)
        <> " / "
        <> T.pack (show nInProg)
        <> " / "
        <> T.pack (show nTot)

    msgGhcMode =
      "GHC Mode: " <> ghcMode <> "\nBackend: " <> backend
      where
        ghcMode = T.pack $ show $ sessionGhcMode sessionInfo
        backend = T.pack $ show $ sessionBackend sessionInfo

    msgsProcessInfo =
      case sessionProcess sessionInfo of
        Nothing -> []
        Just procinfo ->
          [ "Process ID: " <> msgPID
          , "Executable path: " <> msgPath
          , "Current Directory: " <> msgCWD
          ]
            ++ chunkedMsgsArgs
          where
            msgPID = T.pack $ show $ procPID procinfo
            msgPath = T.pack $ procExecPath procinfo
            msgCWD = T.pack $ procCWD procinfo
            msgArgs =
              let mkItem x = T.pack x
               in T.intercalate " " $ fmap mkItem (procArguments procinfo)
            txtArgs = "CLI Arguments: " <> msgArgs
            chunkedMsgsArgs = T.chunksOf 250 txtArgs

    msgsRTSInfo =
      case procRTSFlags <$> sessionProcess sessionInfo of
        Nothing -> []
        Just rtsflags ->
          [ ""
          , "GHC RTS Info"
          , TL.toStrict $ pShowNoColor rtsflags
          ]

    txt =
      T.unlines
        ( [ msgSessionStart
          , ""
          , "Compilation Status"
          , msgCompilationStatus
          , ""
          , msgGhcMode
          ]
            ++ msgsProcessInfo
            ++ msgsRTSInfo
        )

    scene = buildTextView txt []

buildPauseResume :: SessionInfo -> Scene SessionEvent
buildPauseResume session =
  Scene
    { sceneId = "session-button"
    , sceneGlobalViewPort = ViewPort (0, 0) (100, 15)
    , sceneLocalViewPort = ViewPort (0, 0) (100, 15)
    , sceneElements = contents
    , sceneExtent = Nothing
    }
  where
    buttonTxt
      | sessionIsPaused session = "Resume Session"
      | otherwise = "Pause Session"
    hitEvent
      | sessionIsPaused session =
          HitEvent
            { hitEventHoverOn = Nothing
            , hitEventHoverOff = Nothing
            , hitEventClick = Just (Right ResumeSessionEv)
            }
      | otherwise =
          HitEvent
            { hitEventHoverOn = Nothing
            , hitEventHoverOff = Nothing
            , hitEventClick = Just (Right PauseSessionEv)
            }
    contents =
      [ rectangle (0, 0) 100 15 (Just Black) (Just Ivory) (Just 1.0) (Just hitEvent)
      , drawText (5, 0) UpperLeft Sans Black 8 buttonTxt
      ]
