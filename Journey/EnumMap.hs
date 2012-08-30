{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EnumMap (
      EnumMap
    , fromList
    , toList
    , group
    , find
    , lookup
    , keys
    , size
    , mapWithKey
    , filter
    ) where

import Prelude hiding (lookup, filter)

import qualified Data.IntMap as M
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | A sorted collection of associations.
newtype EnumMap e a = MkEnumMap (M.IntMap a) deriving ( Functor
                                                      , Foldable
                                                      , Traversable
                                                      )

instance (Enum e, Show e, Show a) => Show (EnumMap e a) where
  show = show . toList

-- | Create a collection from a list of associations.
fromList :: (Enum e) => [(e, a)] -> EnumMap e a
fromList = MkEnumMap . M.fromListWith const . map assoc
  where assoc (e, v) = (fromEnum e, v)

-- | Group a list of associations creating a collection.
group :: (Enum e) => [(e, a)] -> EnumMap e [a]
group = MkEnumMap . M.fromListWith (++) . map assoc
  where assoc (e, v) = (fromEnum e, [v])

-- | Convert a collection to a sorted list of associations.
toList :: (Enum e) => EnumMap e a -> [(e, a)]
toList (MkEnumMap m) = map assoc $ M.assocs m
  where assoc (e, v) = (toEnum e, v)

-- | Find an element by key.
find :: (Enum e) => e -> EnumMap e a -> a
find e (MkEnumMap m) = m M.! (fromEnum e)

-- | Lookup an element by key.
lookup :: (Enum e) => e -> EnumMap e a -> Maybe a
lookup e (MkEnumMap m) = M.lookup (fromEnum e) m

-- | List of keys.
keys :: (Enum e) => EnumMap e a -> [e]
keys (MkEnumMap m) = map toEnum $ M.keys m

-- | Number of associations.
size :: EnumMap e a -> Int
size (MkEnumMap m) = M.size m

-- | Map a function over all values with the key as extra input.
mapWithKey :: (Enum e) => (e -> a -> b) -> EnumMap e a -> EnumMap e b
mapWithKey f (MkEnumMap m) = MkEnumMap $ M.mapWithKey f' m
  where f' k = f $ toEnum k

filter :: (a -> Bool) -> EnumMap e a -> EnumMap e a
filter f (MkEnumMap m) = MkEnumMap $ M.filter f m
