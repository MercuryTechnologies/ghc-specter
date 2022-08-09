{-# LANGUAGE BangPatterns #-}

module Toolbox.Render.ModuleGraph
  ( renderModuleGraph,
  )
where

import Concur.Core (Widget)
import Concur.Replica (div, pre, text)
import Control.Monad.Extra (loop)
import Data.Discrimination (inner)
import Data.Discrimination.Grouping (grouping)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
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
mkRevDep deps = L.foldl' step emptyMap deps
  where
    emptyMap = L.foldl' (\(!acc) (i, _) -> IM.insert i [] acc) IM.empty deps
    step !acc (i, js) =
      L.foldl' (\(!acc') j -> IM.insertWith (<>) j [i] acc') acc js

analyze :: ModuleGraphInfo -> Text
analyze graphInfo =
  let modDep = mginfoModuleDep graphInfo
      modRevDepMap = mkRevDep modDep
      modRevDep = IM.toList modRevDepMap
      initials = fmap fst $ filter (\(_, js) -> null js) modDep
      terminals = fmap fst $ filter (\(_, js) -> null js) modRevDep
      orphans = initials `L.intersect` terminals
      singles = mapMaybe (\(i, js) -> case js of j : [] -> Just (i, j); _ -> Nothing) modDep
      leg i = loop go ([i], i)
        where
          go (acc', i') =
            case L.lookup i' singles of
              Nothing -> Right acc'
              Just j' -> Left (acc' ++ [j'], j')
      legs = fmap leg (initials L.\\ orphans)

      smallLimit = 20
      modBiDep = concat $ inner grouping joiner fst fst modDep modRevDep
        where
          joiner (i, js) (_, ks) = (i, (js, ks))
      larges = fmap fst $ filter (\(_, (js, ks)) -> length js + length ks > smallLimit) modBiDep
   in "intials: " <> (T.pack $ show initials) <> ",\n"
        <> "terminals: "
        <> (T.pack $ show terminals)
        <> ",\n"
        <> "orphans: "
        <> (T.pack $ show orphans)
        <> ",\n"
        <> "singles: "
        <> (T.pack $ show singles)
        <> ",\n"
        <> "legs: "
        <> (T.pack $ show legs)
        <> "\n=============\n"
        <> "# of larges: "
        <> (T.pack $ show (length larges))

-- | (number of vertices, number of edges)
stat :: ModuleGraphInfo -> (Int, Int)
stat mgi =
  let nVtx = length $ mginfoModuleNameMap mgi
      nEdg = sum $ fmap (length . snd) $ mginfoModuleDep mgi
   in (nVtx, nEdg)

formatModuleGraphInfo :: ModuleGraphInfo -> Text
formatModuleGraphInfo mgi =
  let txt1 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleNameMap mgi
      txt2 =
        T.intercalate "\n" . fmap (T.pack . show) $ mginfoModuleDep mgi
      txt3 =
        T.pack . show $ mginfoModuleTopSorted mgi
      (nVtx, nEdg) = stat mgi
   in "(key, module):\n"
        <> txt1
        <> "\n-----------------\n"
        <> "dependencies:\n"
        <> txt2
        <> "\n-----------------\n"
        <> "top sorted:\n"
        <> txt3
        <> "\n=================\n"
        <> analyze mgi
        <> "\n=================\n"
        <> "# of vertices: "
        <> T.pack (show nVtx)
        <> ", # of edges: "
        <> T.pack (show nEdg)

renderModuleGraph :: ServerState -> Widget HTML a
renderModuleGraph ss =
  let sessionInfo = serverSessionInfo ss
   in case sessionStartTime sessionInfo of
        Nothing ->
          pre [] [text "GHC Session has not been started"]
        Just _ ->
          div
            []
            [ pre [] [text $ formatModuleGraphInfo (sessionModuleGraph sessionInfo)]
            ]
