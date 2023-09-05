{-# LANGUAGE OverloadedRecordDot #-}

module GHCSpecter.Worker.Timing
  ( timingWorker,
    timingBlockerGraphWorker,
  )
where

import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    readTVar,
    writeTVar,
  )
import GHCSpecter.Channel.Outbound.Types
  ( ModuleGraphInfo (..),
    SessionInfo (..),
  )
import GHCSpecter.Data.Timing.Util
  ( makeBlockerGraph,
    makeTimingTable,
  )
import GHCSpecter.Layouter.Graph.Algorithm.Builder (makeRevDep)
import GHCSpecter.Layouter.Graph.Sugiyama qualified as Sugiyama
import GHCSpecter.Server.Types
  ( ModuleGraphState (..),
    ServerState (..),
    TimingState (..),
  )
import GHCSpecter.UI.Types.Event (blockerThreshold)

timingWorker :: TVar ServerState -> IO ()
timingWorker ssRef = do
  ss <- atomically $ readTVar ssRef
  let sessInfo = ss._serverSessionInfo
  case sessionStartTime sessInfo of
    Nothing -> do
      putStrLn "timingWorker something wrong"
      pure ()
    Just sessStart -> do
      let timing = ss._serverTiming._tsTimingMap
          drvModMap = ss._serverDriverModuleMap
          mgi = ss._serverModuleGraphState._mgsModuleGraphInfo
          ttable = makeTimingTable timing drvModMap mgi sessStart
      atomically $ modifyTVar' ssRef $ \ss_ ->
        ss_
          { _serverTiming =
              ss_._serverTiming
                { _tsTimingTable = ttable
                }
          }

timingBlockerGraphWorker :: TVar ServerState -> IO ()
timingBlockerGraphWorker ssRef = do
  (ss', mgi) <-
    atomically $ do
      ss <- readTVar ssRef
      let blThre = blockerThreshold ss._serverTiming._tsBlockerDetailLevel
          mgi = ss._serverModuleGraphState._mgsModuleGraphInfo
          ttable = ss._serverTiming._tsTimingTable
          blockerGraph = makeBlockerGraph blThre mgi ttable
          ss' =
            ss
              { _serverTiming =
                  ss._serverTiming
                    { _tsBlockerGraph = blockerGraph
                    }
              }
      writeTVar ssRef ss'
      pure (ss', mgi)
  let modNameMap = mginfoModuleNameMap mgi
      blockerReversed = makeRevDep (ss'._serverTiming._tsBlockerGraph)
  grVisInfo <- Sugiyama.layOutGraph modNameMap blockerReversed
  atomically $
    modifyTVar' ssRef $ \ss ->
      ss
        { _serverTiming =
            ss._serverTiming
              { _tsBlockerGraphViz = Just grVisInfo
              }
        }
