module GHCSpecter.Util.Map
  ( -- * IsKey class
    IsKey (..),

    -- * KeyMap
    KeyMap,
    emptyKeyMap,
    keyMapToList,
    lookupKey,
    insertToKeyMap,
    alterToKeyMap,

    -- * BiKeyMap
    BiKeyMap,
    emptyBiKeyMap,
    biKeyMapToList,
    forwardLookup,
    backwardLookup,
    insertToBiKeyMap,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (first)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Generics (Generic)
import GHCSpecter.Channel.Common.Types (DriverId (..))

class IsKey k where
  fromKey :: k -> Int
  toKey :: Int -> k

instance IsKey DriverId where
  fromKey = unDriverId
  toKey = DriverId

newtype KeyMap k v = KeyMap {unKeyMap :: IntMap v}
  deriving (Show, Generic)

instance (FromJSON v) => FromJSON (KeyMap k v)

instance (ToJSON v) => ToJSON (KeyMap k v)

emptyKeyMap :: (IsKey k) => KeyMap k v
emptyKeyMap = KeyMap mempty

keyMapToList :: (IsKey k) => KeyMap k v -> [(k, v)]
keyMapToList = fmap (first toKey) . IM.toList . unKeyMap

lookupKey :: (IsKey k) => k -> KeyMap k v -> Maybe v
lookupKey k = IM.lookup (fromKey k) . unKeyMap

insertToKeyMap :: (IsKey k) => (k, v) -> KeyMap k v -> KeyMap k v
insertToKeyMap (k, v) = KeyMap . IM.insert (fromKey k) v . unKeyMap

alterToKeyMap :: (IsKey k) => (Maybe v -> Maybe v) -> k -> KeyMap k v -> KeyMap k v
alterToKeyMap f k = KeyMap . IM.alter f (fromKey k) . unKeyMap

data BiKeyMap k v = BiKeyMap
  { forwardMap :: IntMap v
  , backwardMap :: Map v k
  }
  deriving (Show, Generic)

instance (FromJSON k, FromJSON v, FromJSONKey v, Ord v) => FromJSON (BiKeyMap k v)

instance (ToJSON k, ToJSON v, ToJSONKey v) => ToJSON (BiKeyMap k v)

emptyBiKeyMap :: (IsKey k, Ord v) => BiKeyMap k v
emptyBiKeyMap = BiKeyMap mempty mempty

biKeyMapToList :: (IsKey k) => BiKeyMap k v -> [(k, v)]
biKeyMapToList = fmap (first toKey) . IM.toList . forwardMap

forwardLookup :: (IsKey k) => k -> BiKeyMap k v -> Maybe v
forwardLookup k m = IM.lookup (fromKey k) (forwardMap m)

backwardLookup :: (IsKey k, Ord v) => v -> BiKeyMap k v -> Maybe k
backwardLookup v m = M.lookup v (backwardMap m)

insertToBiKeyMap :: (IsKey k, Ord v) => (k, v) -> BiKeyMap k v -> BiKeyMap k v
insertToBiKeyMap (k, v) m =
  let fwdmap = forwardMap m
      bwdmap = backwardMap m
   in m
        { forwardMap = IM.insert (fromKey k) v fwdmap
        , backwardMap = M.insert v k bwdmap
        }
