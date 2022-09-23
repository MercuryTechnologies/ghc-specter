module GHCSpecter.Worker.CallGraph
  ( -- * top-level decl
    getTopLevelDecls,
    getReducedTopLevelDecls,
    breakSourceText,

    -- * call graph
    makeCallGraph,

    -- * test
    test,
  )
where

import Control.Lens (at, to, (%~), (^.), (^..), (^?), _1, _2, _3, _Just)
import Control.Monad.Trans.State (runState)
import Data.Foldable (for_)
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
import GHCSpecter.Channel (ModuleName)
import GHCSpecter.Server.Types
  ( HasDeclRow' (..),
    HasHieState (..),
    HasModuleHieInfo (..),
    HasRefRow' (..),
    HasServerState (..),
    ModuleHieInfo,
    ServerState (..),
  )
import GHCSpecter.Util.SourceText
  ( filterTopLevel,
    isContainedIn,
    reduceDeclRange,
    splitLineColumn,
  )

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

makeCallGraph ::
  ModuleName ->
  ModuleHieInfo ->
  -- | (decl name, [(unit, module name, ref name)])
  -- if module name is Nothing, it means the current module
  [(Text, [(Text, Maybe ModuleName, Text)])]
makeCallGraph modName modHieInfo = fmap extract topDecls
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

restrictToUnitCallGraph ::
  [(Text, [(Text, Maybe ModuleName, Text)])] ->
  [((Maybe ModuleName, Text), [(Maybe ModuleName, Text)])]
restrictToUnitCallGraph = fmap ((_1 %~ (Nothing,)) . (_2 %~ restrict))
  where
    restrict =
      fmap (\r -> (r ^. _2, r ^. _3))
        . filter (\r -> r ^. _1 . to isInPlace)

makeSymbolMap ::
  [((Maybe ModuleName, Text), [(Maybe ModuleName, Text)])] ->
  IntMap (Maybe ModuleName, Text)
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

test :: ServerState -> IO ()
test ss = do
  putStrLn "test"
  let modName = "Metrics"
      mmodHieInfo = ss ^? serverHieState . hieModuleMap . at modName . _Just
  for_ mmodHieInfo $ \modHieInfo -> do
    let callGraph = restrictToUnitCallGraph $ makeCallGraph modName modHieInfo
        symMap = makeSymbolMap callGraph
        revSymMap = reverseMap symMap
        callGraph' = integerizeGraph revSymMap callGraph
    -- mapM_ print callGraph
    -- mapM_ print $ zip [1..] (gatherSymbols callGraph)
    -- print (makeSymbolMap callGraph)
    print revSymMap
    print callGraph'
