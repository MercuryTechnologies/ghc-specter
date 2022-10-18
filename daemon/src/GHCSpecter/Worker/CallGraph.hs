{-# LANGUAGE TemplateHaskell #-}

module GHCSpecter.Worker.CallGraph
  ( -- * UnitSymbol
    UnitSymbol (..),
    HasUnitSymbol (..),
    ModuleCallGraph (..),
    HasModuleCallGraph (..),

    -- * top-level decl
    getTopLevelDecls,
    getReducedTopLevelDecls,
    breakSourceText,

    -- * call graph
    makeCallGraph,

    -- * layout
    layOutCallGraph,

    -- * worker
    worker,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Lens
  ( makeClassy,
    to,
    (%~),
    (^.),
    (^..),
    _1,
    _2,
    _3,
  )
import Control.Monad.Trans.State (runState)
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List qualified as L
import Data.List.Extra qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.GHC.Hie
  ( HasDeclRow' (..),
    HasModuleHieInfo (..),
    HasRefRow' (..),
    ModuleHieInfo,
  )
import GHCSpecter.GraphLayout.Algorithm.Builder (makeRevDep)
import GHCSpecter.GraphLayout.Sugiyama qualified as Sugiyama
import GHCSpecter.GraphLayout.Types (GraphVisInfo)
import GHCSpecter.Server.Types
  ( HasServerState (..),
    ServerState (..),
    SupplementaryView (..),
  )
import GHCSpecter.Util.SourceText
  ( filterTopLevel,
    isContainedIn,
    reduceDeclRange,
    splitLineColumn,
  )

-- | Symbol only in the current (inplace) unit (package)
data UnitSymbol = UnitSymbol
  { _symModule :: Maybe ModuleName
  , _symName :: Text
  }
  deriving (Eq, Ord, Show)

makeClassy ''UnitSymbol

-- | Call graph inside a module in the current unit scope
-- the index runs from 1 through the number of symbols.
data ModuleCallGraph = ModuleCallGraph
  { _modCallSymMap :: IntMap UnitSymbol
  , _modCallGraph :: IntMap [Int]
  }
  deriving (Show)

makeClassy ''ModuleCallGraph

getTopLevelDecls :: ModuleHieInfo -> [(((Int, Int), (Int, Int)), Text)]
getTopLevelDecls modHieInfo = sortedTopLevelDecls
  where
    extract decl =
      let spos = (decl ^. decl'SLine, decl ^. decl'SCol)
          epos = (decl ^. decl'ELine, decl ^. decl'ECol)
          name = decl ^. decl'NameOcc
       in ((spos, epos), name)
    decls = modHieInfo ^.. modHieDecls . traverse . to extract
    topLevelDecls = filterTopLevel decls
    sortedTopLevelDecls = L.sortBy (compare `on` (^. _1)) topLevelDecls

getReducedTopLevelDecls :: ModuleHieInfo -> [(((Int, Int), (Int, Int)), Text)]
getReducedTopLevelDecls modHieInfo =
  mapMaybe
    (\((start, end), decl) -> (,decl) <$> reduceDeclRange src (start, end) decl)
    topLevelDecls
  where
    src = modHieInfo ^. modHieSource
    topLevelDecls = getTopLevelDecls modHieInfo

breakSourceText :: ModuleHieInfo -> [Text]
breakSourceText modHieInfo = txts ++ [txt]
  where
    src = modHieInfo ^. modHieSource
    topLevelDecls = getTopLevelDecls modHieInfo
    (txts, (_, txt)) = runState (traverse (splitLineColumn . (^. _1 . _1)) topLevelDecls) ((1, 1), src)

makeRawCallGraph ::
  ModuleName ->
  ModuleHieInfo ->
  -- | (decl name, [(unit, module name, ref name)])
  -- if module name is Nothing, it means the current module
  [(Text, [(Text, Maybe ModuleName, Text)])]
makeRawCallGraph modName modHieInfo = fmap extract topDecls
  where
    topDecls = getTopLevelDecls modHieInfo
    allRefs = modHieInfo ^.. modHieRefs . traverse
    extract ((declStart, declEnd), declName) =
      let isHiddenSymbol r =
            "$" `T.isPrefixOf` (r ^. ref'NameOcc)
          isSelf r =
            r ^. ref'NameOcc == declName && r ^. ref'NameMod == modName
          isDepOn r =
            let refStart = (r ^. ref'SLine, r ^. ref'SCol)
                refEnd = (r ^. ref'ELine, r ^. ref'ECol)
             in (refStart, refEnd) `isContainedIn` (declStart, declEnd)

          mkItem r =
            let unitName = r ^. ref'NameUnit
                mmodName =
                  let m = r ^. ref'NameMod
                   in if m == modName then Nothing else Just m
                refName = r ^. ref'NameOcc
             in (unitName, mmodName, refName)

          depRefs =
            filter (\r -> isDepOn r && not (isSelf r) && not (isHiddenSymbol r)) allRefs
          depRefNames = L.nubSort $ fmap mkItem depRefs
       in (declName, depRefNames)

-- NOTE: this may be quite fragile.
-- TODO: check whether this GHC package name convention of self-reference is stable.
isInPlace :: Text -> Bool
isInPlace unitName = "inplace" `T.isSuffixOf` unitName

isMain :: Text -> Bool
isMain = (== "main")

restrictToUnitCallGraph ::
  [(Text, [(Text, Maybe ModuleName, Text)])] ->
  [(UnitSymbol, [UnitSymbol])]
restrictToUnitCallGraph = fmap ((_1 %~ UnitSymbol Nothing) . (_2 %~ restrict))
  where
    restrict =
      fmap (\r -> UnitSymbol (r ^. _2) (r ^. _3))
        . filter (\r -> r ^. _1 . to (\u -> isInPlace u || isMain u))

makeSymbolMap :: [(UnitSymbol, [UnitSymbol])] -> IntMap UnitSymbol
makeSymbolMap callGraph = IM.fromList (zip [1 ..] syms)
  where
    decls = fmap (^. _1) callGraph
    refs = concatMap (^. _2) callGraph
    syms = L.nubSort (decls ++ refs)

reverseMap :: (Ord k) => IntMap k -> Map k Int
reverseMap = M.fromList . fmap swap . IM.toList

integerizeGraph ::
  (Ord k) =>
  Map k Int ->
  [(k, [k])] ->
  Maybe [(Int, [Int])]
integerizeGraph revMap =
  traverse (\(d, rs) -> (,) <$> replace d <*> traverse replace rs)
  where
    replace x = M.lookup x revMap

makeCallGraph :: ModuleName -> ModuleHieInfo -> Maybe ModuleCallGraph
makeCallGraph modName modHieInfo =
  let callGraph0 = restrictToUnitCallGraph $ makeRawCallGraph modName modHieInfo
      symMap = makeSymbolMap callGraph0
      revSymMap = reverseMap symMap
      mcallGraph = IM.fromList <$> integerizeGraph revSymMap callGraph0
   in ModuleCallGraph symMap <$> mcallGraph

maxGraphSize :: Int
maxGraphSize = 300

-- TODO: use appropriate exception types instead of Nothing
layOutCallGraph :: ModuleName -> ModuleHieInfo -> IO (Maybe GraphVisInfo)
layOutCallGraph modName modHieInfo = do
  let mcallGraph = makeCallGraph modName modHieInfo
  case mcallGraph of
    Nothing -> pure Nothing
    Just callGraph -> do
      let symMap = callGraph ^. modCallSymMap
      if IM.size symMap > maxGraphSize
        then pure Nothing
        else do
          let renderSym s =
                let prefix = maybe "" (<> ".") (s ^. symModule)
                 in prefix <> s ^. symName
              labelMap = fmap renderSym symMap
              gr = callGraph ^. modCallGraph . to makeRevDep
          grVis <- Sugiyama.layOutGraph labelMap gr
          pure (Just grVis)

worker :: TVar ServerState -> ModuleName -> ModuleHieInfo -> IO ()
worker var modName modHieInfo = do
  mcallGraphViz <- layOutCallGraph modName modHieInfo
  case mcallGraphViz of
    Nothing -> pure ()
    Just callGraphViz -> do
      let append x Nothing = Just [x]
          append x (Just xs) = Just (xs ++ [x])
      atomically $
        modifyTVar' var $
          serverSuppView
            %~ M.alter (append (("CallGraph", 0), SuppViewCallgraph callGraphViz)) modName
