{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module GHCSpecter.Worker.Hie
  ( hieWorker,
    moduleSourceWorker,
  )
where

import Control.Arrow ((&&&))
import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    writeTQueue,
  )
import Control.Lens ((%~))
import Data.Bifunctor (bimap)
import Data.Foldable (find, for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Monoid (First (..))
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.IO qualified as TIO
import GHC.Iface.Ext.Binary
  ( HieFileResult (..),
    readHieFile,
  )
import GHC.Iface.Ext.Types
  ( BindType (..),
    ContextInfo (..),
    HieFile (..),
    Identifier,
    IdentifierDetails (..),
    Span,
    getAsts,
  )
import GHC.Iface.Ext.Utils (generateReferencesMap)
import GHC.Types.Name (nameModule_maybe, nameOccName, nameSrcSpan, occNameString)
import GHC.Types.Name.Cache (initNameCache)
import GHC.Types.SrcLoc
  ( SrcSpan (RealSrcSpan),
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
  )
import GHC.Unit.Types (GenModule (..), Unit, moduleName)
import GHCSpecter.Channel.Common.Types (ModuleName)
import GHCSpecter.Data.GHC.Hie
  ( DeclRow' (..),
    DefRow' (..),
    ModuleHieInfo (..),
    RefRow' (..),
    emptyModuleHieInfo,
  )
import GHCSpecter.Server.Types
  ( HasHieState (..),
    HasServerState (..),
    ServerState (..),
  )
import GHCSpecter.Util.GHC (moduleNameString)
import GHCSpecter.Worker.CallGraph qualified as CallGraph

genRefsAndDecls ::
  FilePath ->
  GenModule Unit ->
  M.Map Identifier [(Span, IdentifierDetails a)] ->
  ([RefRow'], [DeclRow'])
genRefsAndDecls path smdl refmap = genRows $ flat $ M.toList refmap
  where
    flat = concatMap (\(a, xs) -> map (a,) xs)
    genRows = foldMap go
    go = bimap maybeToList maybeToList . (goRef &&& goDec)

    goRef (Right name, (sp, _))
      | Just modu <- nameModule_maybe name =
          Just $
            RefRow'
              { _ref'Src = path,
                _ref'NameOcc = T.pack $ occNameString occ,
                _ref'NameMod = T.pack $ moduleNameString $ moduleName modu,
                _ref'NameUnit = T.pack $ show $ moduleUnit modu,
                _ref'SLine = sl,
                _ref'SCol = sc,
                _ref'ELine = el,
                _ref'ECol = ec
              }
      where
        occ = nameOccName name
        sl = srcSpanStartLine sp
        sc = srcSpanStartCol sp
        el = srcSpanEndLine sp
        ec = srcSpanEndCol sp
    goRef _ = Nothing

    goDec (Right name, (_, dets))
      | Just modu <- nameModule_maybe name,
        modu == smdl,
        occ <- nameOccName name,
        info <- identInfo dets,
        Just sp <- getBindSpan info,
        is_root <- isRoot info,
        sl <- srcSpanStartLine sp,
        sc <- srcSpanStartCol sp,
        el <- srcSpanEndLine sp,
        ec <- srcSpanEndCol sp =
          Just $
            DeclRow'
              { _decl'Src = path,
                _decl'NameOcc = T.pack $ occNameString occ,
                _decl'SLine = sl,
                _decl'SCol = sc,
                _decl'ELine = el,
                _decl'ECol = ec,
                _decl'Root = is_root
              }
    goDec _ = Nothing

    isRoot =
      any
        ( \case
            ValBind InstanceBind _ _ -> True
            Decl _ _ -> True
            _ -> False
        )

    getBindSpan = getFirst . foldMap (First . goDecl)
    goDecl (ValBind _ _ sp) = sp
    goDecl (PatternBind _ _ sp) = sp
    goDecl (Decl _ sp) = sp
    goDecl (RecField _ sp) = sp
    goDecl _ = Nothing

genDefRow ::
  FilePath ->
  GenModule Unit ->
  M.Map Identifier [(Span, IdentifierDetails a)] ->
  [DefRow']
genDefRow path smod refmap = genRows $ M.toList refmap
  where
    genRows = mapMaybe go
    getSpan name dets
      | RealSrcSpan sp _ <- nameSrcSpan name = Just sp
      | otherwise = do
          (sp, _dets) <- find defSpan dets
          pure sp

    defSpan = any isDef . identInfo . snd
    isDef (ValBind RegularBind _ _) = True
    isDef PatternBind {} = True
    isDef Decl {} = True
    isDef _ = False

    go (Right name, dets)
      | Just modu <- nameModule_maybe name,
        modu == smod,
        occ <- nameOccName name,
        Just sp <- getSpan name dets,
        sl <- srcSpanStartLine sp,
        sc <- srcSpanStartCol sp,
        el <- srcSpanEndLine sp,
        ec <- srcSpanEndCol sp =
          Just $
            DefRow'
              { _def'Src = path,
                _def'NameOcc = T.pack $ occNameString occ,
                _def'SLine = sl,
                _def'SCol = sc,
                _def'ELine = el,
                _def'ECol = ec
              }
    go _ = Nothing

hieWorker :: TVar ServerState -> TQueue (IO ()) -> FilePath -> IO ()
hieWorker ssRef workQ hiefile = do
  nc <- initNameCache 'z' []
  hieResult <- readHieFile nc hiefile
  let hf = hie_file_result hieResult
      src = decodeUtf8With (\_ _ -> Just ' ') $ hie_hs_src hf
      modu = hie_module hf
      modName = T.pack $ moduleNameString $ moduleName modu
      asts = hie_asts hf
      refmap = generateReferencesMap $ getAsts asts
      (refs, decls) = genRefsAndDecls "" modu refmap
      defs = genDefRow "" modu refmap
      modHie =
        emptyModuleHieInfo
          { _modHieRefs = refs,
            _modHieDecls = decls,
            _modHieDefs = defs,
            _modHieSource = src
          }
      callGraphWork = CallGraph.worker ssRef modName modHie
  atomically $ do
    modifyTVar' ssRef $
      serverHieState . hieModuleMap
        %~ M.insert modName modHie
    writeTQueue workQ callGraphWork

moduleSourceWorker :: TVar ServerState -> Map ModuleName FilePath -> IO ()
moduleSourceWorker ssRef modSrcs = do
  for_ (M.toList modSrcs) $ \(modu, srcFile) -> do
    src <- TIO.readFile srcFile
    let update Nothing = Just (emptyModuleHieInfo {_modHieSource = src})
        update (Just modHie) = Just (modHie {_modHieSource = src})
    atomically $
      modifyTVar' ssRef (serverHieState . hieModuleMap %~ M.alter update modu)
