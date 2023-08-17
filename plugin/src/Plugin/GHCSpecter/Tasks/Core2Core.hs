{-# LANGUAGE OverloadedStrings #-}

module Plugin.GHCSpecter.Tasks.Core2Core
  ( listCore,
    printCore,
    --
    getContent,
  )
where

import Control.Error.Util (note)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Short qualified as SB
import Data.Data (Data (..), cast, dataTypeName)
import Data.List qualified as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Core (Bind (NonRec, Rec))
import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.DataCon (DataCon)
import GHC.Core.Opt.Monad (CoreM, getDynFlags)
import GHC.Core.PatSyn (PatSyn)
import GHC.Core.TyCon (TyCon)
import GHC.Data.FastString (FastString (..))
import GHC.Driver.Session (DynFlags)
import GHC.Types.Name (Name, NamedThing (..), OccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan)
import GHC.Types.Var (Var)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Unit.Types (Unit, toUnitId, unitString)
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
import GHCSpecter.Util.GHC
  ( ModuleName,
    moduleNameString,
    printPpr,
    showPpr,
  )

getOccNameDynamically ::
  forall t a.
  (Typeable t, NamedThing t, Data a) =>
  Proxy t ->
  a ->
  Maybe Text
getOccNameDynamically _ x =
  let my :: Maybe t
      my = cast x
   in fmap (T.pack . occNameString . getOccName) my

getNameDynamically ::
  forall t a.
  (Typeable t, NamedThing t, Data a) =>
  Proxy t ->
  DynFlags ->
  a ->
  Maybe Text
getNameDynamically _ dflags x =
  let my :: Maybe t
      my = cast x
   in fmap (T.pack . showPpr dflags . getName) my

-- NOTE: a few data types used in Core has partial toConstr (Data.ByteString.ByteString)
-- or abstractConstr, which causes an exception or not inspectable expression
-- after conversion.
-- Many of them contain stringy information (usually name), so we extract the names
-- in such cases and do not proceed the conversion to the children of the node.

-- | Left: ordinary node, Right: extract and do not proceed to the children
getContent :: forall a. Data a => DynFlags -> a -> (Text, Either Text (Maybe Text))
getContent dflags x = (T.pack dtypName, evalue)
  where
    dtyp = dataTypeOf x
    dtypName = dataTypeName dtyp
    evalue
      | dtypName == "Data.ByteString.ByteString" =
          let mbs = cast x
           in Right (fmap decodeUtf8 mbs)
      -- unclear how to treat Bag
      | dtypName == "Bag" = Right Nothing
      | dtypName == "Class" = Right $ getOccNameDynamically (Proxy @Class) x
      -- unclear how to handle CoAxiom
      | dtypName == "CoAxiom" = Right Nothing
      -- unclear how to treat CoAxiomRule (coaxrProves is a function)
      | dtypName == "CoAxiomRule" = Right Nothing
      -- unclear how to treat CoercionHole (ch_ref :: IORef)
      | dtypName == "CoercionHole" = Right Nothing
      | dtypName == "ConLike" = Right $ getOccNameDynamically (Proxy @ConLike) x
      | dtypName == "DataCon" = Right $ getOccNameDynamically (Proxy @DataCon) x
      | dtypName == "FastString" =
          let my = cast x
           in Right (fmap (decodeUtf8 . SB.fromShort . fs_sbs) my)
      -- unclear how to treat HoleExprRef
      | dtypName == "HoleExprRef" = Right Nothing
      | dtypName == "ModuleName" =
          let my :: Maybe ModuleName = cast x
           in Right $ fmap (T.pack . moduleNameString) my
      | dtypName == "Name" = Right $ getOccNameDynamically (Proxy @Name) x
      | dtypName == "OccName" =
          let my :: Maybe OccName = cast x
           in Right $ fmap (T.pack . occNameString) my
      | dtypName == "PatSyn" = Right $ getOccNameDynamically (Proxy @PatSyn) x
      | dtypName == "RealSrcSpan" =
          let my :: Maybe RealSrcSpan = cast x
           in Right $ fmap (T.pack . show) my
      | dtypName == "SrcSpan" =
          let my :: Maybe SrcSpan = cast x
           in Right $ fmap (T.pack . show) my
      -- unclear how to treat TcEvBinds
      | dtypName == "TcEvBinds" = Right Nothing
      | dtypName == "TyCon" = Right $ getOccNameDynamically (Proxy @TyCon) x
      | dtypName == "Unit" =
          let my :: Maybe Unit = cast x
           in Right $ fmap (T.pack . unitString . toUnitId) my
      -- NOTE: OccName is not enough for distinguishing Var's.
      -- Using ppr, "_(uniq symbol)" suffix is added.
      | dtypName == "Var" = Right $ getNameDynamically (Proxy @Var) dflags x
      | otherwise = Left (T.pack (show (toConstr x)))

listCore :: ModGuts -> CoreM ConsoleReply
listCore guts = do
  dflags <- getDynFlags
  let binds = mg_binds guts
      extractName =
        note "Error in getNameDynamically"
          . getNameDynamically (Proxy @Var) dflags
      formatBind (NonRec t _) = (\n -> [n]) <$> extractName t
      formatBind (Rec bs) = traverse (extractName . fst) bs
      reply =
        case traverse formatBind binds of
          Left err ->
            ConsoleReplyText Nothing ("Error: " <> err)
          Right bindList ->
            ConsoleReplyCoreBindList bindList
  pure reply

printCore :: ModGuts -> [Text] -> CoreM ConsoleReply
printCore guts args = do
  dflags <- getDynFlags
  let -- check whether a bind is requested by user
      isReq (NonRec t _) =
        let name = fromMaybe "#######" $ getNameDynamically (Proxy @Var) dflags t
         in name `L.elem` args
      isReq (Rec bs) =
        let names = mapMaybe (getNameDynamically (Proxy @Var) dflags . fst) bs
         in not (null (names `L.intersect` args))
      binds = mg_binds guts
      binds' = filter isReq binds
      txt = T.pack (showPpr dflags binds')
  -- for debug
  mapM_ (liftIO . printPpr dflags) binds'
  pure (ConsoleReplyText (Just "core") txt)

-- NOTE: we disable the old direct rendering for now.
-- let forest = fmap (core2tree dflags) binds'
-- pure (ConsoleReplyCore forest)
