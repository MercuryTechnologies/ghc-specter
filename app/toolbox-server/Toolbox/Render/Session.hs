{-# LANGUAGE BangPatterns #-}

module Toolbox.Render.Session
  ( renderSession,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Replica.VDOM.Types (HTML)
import Toolbox.Channel
  ( ModuleGraphInfo (..),
    SessionInfo (..),
  )
import Toolbox.Server.Types (ServerState (..))
import Prelude hiding (div)

mkRevDep :: [(Int, [Int])] -> IntMap [Int]
mkRevDep = foldl' step IM.empty
  where
    step !acc (i, js) =
      foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      revDepMap = mkRevDep modDep
      orphans = filter (\(_, js) -> null js) modDep
      nOrphans = length orphans
      nSingle = length $ filter (\(_, js) -> length js == 1) modDep
   in (T.pack $ show nOrphans) <> ", " <> (T.pack $ show nSingle)
        <> "\n=============\n"
        <> (T.pack $ show revDepMap)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt0 = analyze mgi
      txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleDep mgi
   in txt0
        <> "\n-----------------\n"
        <> "(key, module):\n"
        <> txt1
        <> "\n-----------------\n"
        <> "dependencies:\n"
        <> txt2

renderSession :: ServerState -> Widget HTML a
renderSession ss =
  let sessionInfo = serverSessionInfo ss
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just sessionStartTime ->
          div
            []
            [ pre [] [text $ T.pack $ show sessionStartTime]
            , pre [] [text $ formatModuleGraphInfo (sessionModuleGraph sessionInfo)]
            ]
