{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.GHCSpecter.Hooks
  ( -- * send information
    sendModuleStart,
    sendModuleName,

    -- * hooks
    runRnSpliceHook',
    runMetaHook',
    runPhaseHook',
  )
where

import Control.Monad.Extra (ifM)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Core.Opt.Monad (getDynFlags)
import GHC.Data.IOEnv (getEnv)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Pipeline (runPhase)
import GHC.Driver.Plugins
  ( PluginWithArgs (..),
    StaticPlugin (..),
  )
import GHC.Driver.Session (DynFlags)
import GHC.Hs.Extension (GhcRn)
import GHC.Stats
  ( GCDetails (..),
    RTSStats (..),
    getRTSStats,
    getRTSStatsEnabled,
  )
import GHC.Tc.Gen.Splice (defaultRunMeta)
import GHC.Tc.Types (Env (..), RnM, TcM)
import GHC.Types.Meta (MetaHook, MetaRequest (..), MetaResult)
import GHC.Utils.Outputable (Outputable)
import GHCSpecter.Channel.Common.Types (DriverId (..), type ModuleName)
import GHCSpecter.Channel.Outbound.Types
  ( BreakpointLoc (..),
    ChanMessage (..),
    MemInfo (..),
    Timer (..),
    TimerTag (..),
  )
import Language.Haskell.Syntax.Expr (HsSplice)
import Plugin.GHCSpecter.Comm (queueMessage)
import Plugin.GHCSpecter.Console (breakPoint)
import Plugin.GHCSpecter.Tasks
  ( postMetaCommands,
    postPhaseCommands,
    preMetaCommands,
    prePhaseCommands,
    rnSpliceCommands,
  )
import Safe (readMay)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (setAllocationCounter, getAllocationCounter)
-- GHC version dependent imports
#if MIN_VERSION_ghc(9, 4, 0)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Data.Foldable (for_)
import Data.Text (Text)
import GHC.Driver.Pipeline (PipeEnv (..))
import GHC.Driver.Pipeline.Phases
  ( PhaseHook (..),
    TPhase (..),
  )
import GHC.Driver.Plugins (staticPlugins)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GHC.Unit.Module.Name qualified as GHC
import GHCSpecter.Data.Map (backwardLookup, insertToBiKeyMap)
import Plugin.GHCSpecter.Types
  ( PluginSession (..),
    getModuleFromDriverId,
    getModuleFileFromDriverId,
    sessionRef,
  )
import Plugin.GHCSpecter.Util (getModuleName)
import System.Directory (canonicalizePath)
#elif MIN_VERSION_ghc(9, 2, 0)
import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Phases (Phase (As, StopLn))
import GHC.Driver.Pipeline.Monad
  ( CompPipeline,
    PhasePlus (HscOut, RealPhase),
    getPipeSession,
  )
import GHCSpecter.Util.GHC (showPpr)
#endif


data PhasePoint = PhaseStart | PhaseEnd

-- NOTE: always assume a single static plugin, the first arg = drvId
getDriverIdFromHscEnv :: HscEnv -> Maybe DriverId
getDriverIdFromHscEnv hscenv = do
#if MIN_VERSION_ghc(9, 4, 0)
  sp <- listToMaybe (staticPlugins (hsc_plugins hscenv))
  arg1 <- listToMaybe (paArguments (spPlugin sp))
  DriverId <$> readMay arg1
#elif MIN_VERSION_ghc(9, 2, 0)
  sp <- listToMaybe (hsc_static_plugins hscenv)
  arg1 <- listToMaybe (paArguments (spPlugin sp))
  DriverId <$> readMay arg1
#endif

sendModuleStart ::
  DriverId ->
  UTCTime ->
  Maybe MemInfo ->
  IO ()
sendModuleStart drvId startTime mmeminfo = do
  let timer = Timer [(TimerStart, (startTime, mmeminfo))]
  queueMessage (CMTiming drvId timer)

sendModuleName ::
  DriverId ->
  ModuleName ->
  Maybe FilePath ->
  IO ()
sendModuleName drvId modName msrcfile =
  queueMessage (CMModuleInfo drvId modName msrcfile)

runRnSpliceHook' :: HsSplice GhcRn -> RnM (HsSplice GhcRn)
runRnSpliceHook' splice = do
  mdrvId <- getDriverIdFromHscEnv . env_top <$> getEnv
  case mdrvId of
    Nothing -> pure splice
    Just drvId -> do
      breakPoint drvId RnSplice (rnSpliceCommands splice)
      pure splice

-- NOTE: This is a HACK. The constructors of MetaResult are deliberately
-- not exposed, and therefore, runMeta wraps runMetaHook with internal
-- case pattern match corresponding to MetaRequest.
-- We circumvent this problem by replacing the wrapper function provided
-- by MetaRequest constructors with our custom function with a breakpoint.
-- Unfortunately, the MetaRequest constructor functions are pure functions,
-- so the ugly unsafePerformIO hack is needed here.
wrapMeta ::
  (Outputable s) =>
  (s -> MetaResult) ->
  DriverId ->
  DynFlags ->
  s ->
  MetaResult
wrapMeta unMeta drvId dflags s =
  unsafePerformIO $ do
    breakPoint drvId PostRunMeta (postMetaCommands dflags s)
    pure (unMeta s)
{-# NOINLINE wrapMeta #-}

runMetaHook' :: MetaHook TcM
runMetaHook' metaReq expr = do
  mdrvId <- getDriverIdFromHscEnv . env_top <$> getEnv
  case mdrvId of
    Nothing -> defaultRunMeta metaReq expr
    Just drvId -> do
      dflags <- getDynFlags
      breakPoint drvId PreRunMeta (preMetaCommands expr)
      -- HACK: as constructors of MetaResult are not exported, this is the only way.
      let metaReq' =
            case metaReq of
              MetaE r -> MetaE (wrapMeta r drvId dflags)
              MetaP r -> MetaP (wrapMeta r drvId dflags)
              MetaT r -> MetaT (wrapMeta r drvId dflags)
              MetaD r -> MetaD (wrapMeta r drvId dflags)
              MetaAW r -> MetaAW (wrapMeta r drvId dflags)
      defaultRunMeta metaReq' expr

#if MIN_VERSION_ghc(9, 4, 0)
tphase2Text :: TPhase res -> Text
tphase2Text p =
  case p of
    T_Unlit {} -> "T_Unlit"
    T_FileArgs {} -> "T_FileArgs"
    T_Cpp {} -> "T_Cpp"
    T_HsPp {} -> "T_HsPp"
    T_HscRecomp {} -> "T_HsRecomp"
    T_Hsc {} -> "T_Hsc"
    T_HscPostTc {} -> "T_HscPostTc"
    T_HscBackend {} -> "T_HscBackend"
    T_CmmCpp {} -> "T_CmmCpp"
    T_Cmm {} -> "T_Cmm"
    T_Cc {} -> "T_Cc"
    T_As {} -> "T_As"
    T_LlvmOpt {} -> "T_LlvmOpt"
    T_LlvmLlc {} -> "T_LlvmLlc"
    T_LlvmMangle {} -> "T_LlvmMangle"
    T_MergeForeign {} -> "T_MergeForeign"

envFromTPhase :: TPhase res -> (HscEnv, Maybe PipeEnv, Maybe ModuleName)
envFromTPhase p =
  case p of
    T_Unlit penv env _ -> (env, Just penv, Nothing)
    T_FileArgs env _ -> (env, Nothing, Nothing)
    T_Cpp penv env _ -> (env, Just penv, Nothing)
    T_HsPp penv env _ _ -> (env, Just penv, Nothing)
    T_HscRecomp penv env _ _ -> (env, Just penv, Nothing)
    T_Hsc env modSummary -> (env, Nothing, Just (getModuleName modSummary))
    T_HscPostTc env modSummary _ _ _ -> (env, Nothing, Just (getModuleName modSummary))
    T_HscBackend penv env mname _ _ _ -> (env, Just penv, Just (T.pack (GHC.moduleNameString mname)))
    T_CmmCpp penv env _ -> (env, Just penv, Nothing)
    T_Cmm penv env _ -> (env, Just penv, Nothing)
    T_Cc _ penv env _ -> (env, Just penv, Nothing)
    T_As _ penv env _ _ -> (env, Just penv, Nothing)
    T_LlvmOpt penv env _ -> (env, Just penv, Nothing)
    T_LlvmLlc penv env _ -> (env, Just penv, Nothing)
    T_LlvmMangle penv env _ -> (env, Just penv, Nothing)
    T_MergeForeign penv env _ _ -> (env, Just penv, Nothing)

issueNewDriverId :: ModSummary -> IO DriverId
issueNewDriverId modSummary = do
  atomically $ do
    s <- readTVar sessionRef
    let drvId = psNextDriverId s
        modName = getModuleName modSummary
        mmodFile = ml_hs_file $ ms_location modSummary

        drvModMap = psDrvIdModuleMap s
        drvModMap' = insertToBiKeyMap (drvId, modName) drvModMap
        drvModFileMap = psDrvIdModuleFileMap s
        drvModFileMap' =
          case mmodFile of
            Nothing -> drvModFileMap
            Just modFile -> insertToBiKeyMap (drvId, modFile) drvModFileMap
        s' =
          s { psDrvIdModuleMap = drvModMap'
            , psDrvIdModuleFileMap = drvModFileMap'
            , psNextDriverId = drvId + 1
            }
    writeTVar sessionRef s'
    pure drvId

getMemInfo :: IO (Maybe MemInfo)
getMemInfo = ifM getRTSStatsEnabled getInfo (pure Nothing)
  where
    getInfo = do
      rtsstats <- getRTSStats
      alloc <- getAllocationCounter
      let liveBytes = gcdetails_live_bytes . gc $ rtsstats
      pure $ Just $ MemInfo liveBytes alloc

-- NOTE: I tried to approximate GHC 9.2 version of this function.
-- TODO: Figure out more robust way for detecting the end of
-- compilation. In the worst case, we can just turn on timing
-- log and parse the message on the fly.
sendCompStateOnPhase ::
  DriverId ->
  TPhase r ->
  PhasePoint ->
  IO ()
sendCompStateOnPhase drvId phase pt = do
  case phase of
    T_Unlit {} -> pure ()
    T_FileArgs {} -> pure ()
    T_Cpp {} -> pure ()
    T_HsPp {} -> pure ()
    T_HscRecomp {} -> pure ()
    T_Hsc {} ->
      case pt of
        PhaseStart -> do
          -- send timing information
          startTime <- getCurrentTime
          setAllocationCounter 0
          mmeminfo <- getMemInfo
          sendModuleStart drvId startTime mmeminfo
          -- send module name information
          mmodName <- atomically $ getModuleFromDriverId drvId
          msrcFile <- atomically $ getModuleFileFromDriverId drvId
          msrcFile' <- traverse canonicalizePath msrcFile
          for_ mmodName $ \modName ->
            sendModuleName drvId modName msrcFile'
        _ -> pure ()
    T_HscPostTc {} ->
      case pt of
        PhaseEnd -> do
          -- send timing information
          hscOutTime <- getCurrentTime
          mmeminfo <- getMemInfo
          let timer = Timer [(TimerHscOut, (hscOutTime, mmeminfo))]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    T_HscBackend {} -> pure ()
    T_CmmCpp {} -> pure ()
    T_Cmm {} -> pure ()
    T_Cc {} -> pure ()
    T_As {} ->
      case pt of
        PhaseStart -> do
          -- send timing information
          asStartTime <- getCurrentTime
          mmeminfo <- getMemInfo
          let timer = Timer [(TimerAs, (asStartTime, mmeminfo))]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    T_LlvmOpt {} -> pure ()
    T_LlvmLlc {} -> pure ()
    T_LlvmMangle {} -> pure ()
    T_MergeForeign {} ->
      case pt of
        PhaseEnd -> do
          -- send timing information
          endTime <- getCurrentTime
          mmeminfo <- getMemInfo
          let timer = Timer [(TimerEnd, (endTime, mmeminfo))]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()

runPhaseHook' :: PhaseHook
runPhaseHook' = PhaseHook $ \phase -> do
  let (_, mpenv, mname) = envFromTPhase phase
      phaseTxt = tphase2Text phase
  (phase', mdrvId) <-
    case phase of
      T_Hsc env modSummary -> do
        mdrvId' <- Just <$> issueNewDriverId modSummary
        let -- NOTE: This rewrite all the arguments regardless of what the plugin is.
           -- TODO: find way to update the plugin options only for ghc-specter-plugin.
           provideContext (StaticPlugin pa) =
             let pa' = pa {paArguments = maybe [] (\x -> [show (unDriverId x)]) mdrvId'}
              in StaticPlugin pa'
           plugins = hsc_plugins env
           splugins = staticPlugins plugins
           splugins' = fmap provideContext splugins
           plugins' = plugins {staticPlugins = splugins'}
           env' = env {hsc_plugins = plugins'}
           phase' = T_Hsc env' modSummary
        pure (phase', mdrvId')
      _ -> atomically $ do
        s <- readTVar sessionRef
        let mdrvId' =
              (mname >>= \name -> backwardLookup name (psDrvIdModuleMap s))
              <|> ( mpenv >>= \(PipeEnv {..}) ->
                      backwardLookup src_filename (psDrvIdModuleFileMap s)
                  )
        pure (phase, mdrvId')
  case mdrvId of
    Nothing -> runPhase phase'
    Just drvId -> do
      let locPrePhase = PreRunPhase phaseTxt
      breakPoint drvId locPrePhase prePhaseCommands
      sendCompStateOnPhase drvId phase PhaseStart
      result <- runPhase phase'
      let phase'Txt = phaseTxt
          locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
      breakPoint drvId locPostPhase postPhaseCommands
      sendCompStateOnPhase drvId phase PhaseEnd
      pure result

#elif MIN_VERSION_ghc(9, 2, 0)
sendCompStateOnPhase ::
  DriverId ->
  PhasePlus ->
  PhasePoint ->
  CompPipeline ()
sendCompStateOnPhase drvId phase pt = do
  case phase of
    RealPhase StopLn -> liftIO do
      -- send timing information
      endTime <- getCurrentTime
      let timer = Timer [(TimerEnd, endTime)]
      queueMessage (CMTiming drvId timer)
    RealPhase (As _) ->
      case pt of
        PhaseStart -> liftIO $ do
          -- send timing information
          endTime <- getCurrentTime
          let timer = Timer [(TimerAs, endTime)]
          queueMessage (CMTiming drvId timer)
        _ -> pure ()
    HscOut _ _ _ -> liftIO $ do
      -- send timing information
      hscOutTime <- getCurrentTime
      let timer = Timer [(TimerHscOut, hscOutTime)]
      queueMessage (CMTiming drvId timer)
    _ -> pure ()

runPhaseHook' ::
  PhasePlus ->
  FilePath ->
  CompPipeline (PhasePlus, FilePath)
runPhaseHook' phase fp = do
  hscenv <- getPipeSession
  let mdrvId = getDriverIdFromHscEnv hscenv
  case mdrvId of
    Nothing -> runPhase phase fp
    Just drvId -> do
      dflags <- getDynFlags
      -- pre phase timing
      let phaseTxt = T.pack (showPpr dflags phase)
          locPrePhase = PreRunPhase phaseTxt
      breakPoint drvId locPrePhase prePhaseCommands
      sendCompStateOnPhase drvId phase PhaseStart
      -- actual runPhase
      (phase', fp') <- runPhase phase fp
      -- post phase timing
      let phase'Txt = T.pack (showPpr dflags phase')
          locPostPhase = PostRunPhase (phaseTxt, phase'Txt)
      breakPoint drvId locPostPhase postPhaseCommands
      sendCompStateOnPhase drvId phase' PhaseEnd
      pure (phase', fp')
#endif
