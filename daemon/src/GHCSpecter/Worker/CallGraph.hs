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

import Control.Lens (at, to, (^.), (^..), (^?), _1, _Just)
import Control.Monad.Trans.State (runState)
import Data.Foldable (for_)
import Data.Function (on)
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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
  [(Text, [(ModuleName, Text)])]
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

          depRefs =
            filter (\r -> isDepOn r && not (isSelf r) && not (isHiddenSymbol r)) allRefs
          depRefNames = L.nub $ L.sort $ fmap (\r -> (r ^. ref'NameMod, r ^. ref'NameOcc)) depRefs
       in (declName, depRefNames)

test :: ServerState -> IO ()
test ss = do
  putStrLn "test"
  let modName = "Metrics"
      mmodHieInfo = ss ^? serverHieState . hieModuleMap . at modName . _Just
  for_ mmodHieInfo $ \modHieInfo -> do
    let callGraph = makeCallGraph modName modHieInfo
    mapM_ print callGraph
