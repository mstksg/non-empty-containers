{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Map.NonEmpty (
  -- * Map type
  NonEmptyMap(..)
  -- , toNonEmpty
  , nonEmptyMap
  , toMap
  , insertMap
  , insertMapWith
  , insertMapWithKey

  -- * Construction
  , empty
  , singleton
  , fromSet

  -- ** From Unordered Lists
  , fromList
  , fromListWith
  , fromListWithKey

  -- ** From Ascending Lists
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

  -- -- ** From Descending Lists
  -- , fromDescList
  -- , fromDescListWith
  -- , fromDescListWithKey
  -- , fromDistinctDescList

  -- * Insertion
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey

  -- * Deletion\/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , alterF

  -- * Query
  -- ** Lookup
  , lookup
  , (!?)
  , (!)
  , findWithDefault
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE

  -- ** Size
  , null
  , size

  -- * Combine

  -- ** Union
  , union
  , unionWith
  , unionWithKey
  -- , unions
  -- , unionsWith

  -- -- ** Difference
  -- , difference
  -- , (\\)
  -- , differenceWith
  -- , differenceWithKey

  -- -- ** Intersection
  -- , intersection
  -- , intersectionWith
  -- , intersectionWithKey

  -- -- ** Unsafe general combining function
  -- , mergeWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
  , traverseMaybeWithKey
  , mapAccum
  , mapAccumWithKey
  -- , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , mapKeysMonotonic

  -- * Folds
  -- , foldr
  , foldl
  -- , foldrWithKey
  -- , foldlWithKey
  , foldMapWithKey

  -- ** Strict folds
  -- , foldr'
  , foldr1'
  -- , foldl'
  -- , foldrWithKey'
  -- , foldlWithKey'

  -- * Conversion
  , elems
  , keys
  -- , assocs
  , keysSet

  -- ** Lists
  , toNonEmpty

  -- ** Ordered lists
  -- , toAscList
  -- , toDescList

  -- * Filter
  , filter
  , filterWithKey
  , restrictKeys
  , withoutKeys
  , partition
  , partitionWithKey
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone

  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey

  , split
  , splitLookup

  -- * Submap
  , isSubmapOf, isSubmapOfBy
  , isProperSubmapOf, isProperSubmapOfBy

  -- -- * Indexed
  -- , lookupIndex
  -- , findIndex
  -- , elemAt
  -- , updateAt
  -- , deleteAt
  -- , take
  -- , drop
  -- , splitAt

  -- * Min\/Max
  , lookupMin
  , lookupMax
  -- , findMin
  -- , findMax
  , deleteMin
  , deleteMax
  -- , deleteFindMin
  -- , deleteFindMax
  , updateMin
  , updateMax
  , updateMinWithKey
  , updateMaxWithKey
  , minView
  , maxView
  , minViewWithKey
  , maxViewWithKey
  , valid
  ) where

import Prelude hiding (lookup,foldr1,foldr,filter,map)
import Control.Applicative
import Data.Bifunctor
import Data.Bool (bool)
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Traversable (Traversable1(traverse1,sequence1))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Functor.Apply
import Data.Function
import Data.Maybe hiding (mapMaybe)
import Data.Set.NonEmpty (NonEmptySet(..))

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.Semigroup.Foldable
import qualified Data.Semigroup.Traversable
import qualified Data.List.NonEmpty as NE

-- | A non-empty map.
data NonEmptyMap k v =
    NonEmptyMap !k   -- invariant: must be smaller than smallest key in map
                v
                !(M.Map k v)
  deriving (Eq,Ord,Functor)

-- | A map with a single element.
singleton :: k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap k v M.empty

fromSet :: (k -> v) -> NonEmptySet k -> NonEmptyMap k v
fromSet f (NonEmptySet k ks) = NonEmptyMap k (f k) (M.fromSet f ks)

-- | Lookup the value at a key in the map.
lookup :: Ord k => k -> NonEmptyMap k a -> Maybe a
lookup a (NonEmptyMap k v xs) = case compare a k of
  LT -> Nothing
  EQ -> Just v
  GT -> M.lookup k xs

(!?) :: Ord k => NonEmptyMap k v -> k -> Maybe v
m !? k = lookup k m

(!) :: Ord k => NonEmptyMap k v -> k -> v
(!) m k = fromMaybe e $ m !? k
  where
    e = error "NonEmptyMap.!: given key is not an element in the map"

findWithDefault :: Ord k => v -> k -> NonEmptyMap k v -> v
findWithDefault def a (NonEmptyMap k v xs) = case compare a k of
  LT -> def
  EQ -> v
  GT -> M.findWithDefault def k xs

member :: Ord k => k -> NonEmptyMap k v -> Bool
member a (NonEmptyMap k _ xs) = case compare a k of
  LT -> False
  EQ -> True
  GT -> M.member a xs

notMember :: Ord k => k -> NonEmptyMap k v -> Bool
notMember a = not . member a

lookupLT :: Ord k => k -> NonEmptyMap k v -> Maybe (k, v)
lookupLT a (NonEmptyMap k v xs) = case compare a k of
  LT -> Nothing
  EQ -> Nothing
  GT -> M.lookupLT k xs <|> Just (k, v)

lookupGT :: Ord k => k -> NonEmptyMap k v -> Maybe (k, v)
lookupGT a (NonEmptyMap k v xs) = case compare a k of
  LT -> Just (k, v)
  EQ -> M.lookupGT k xs
  GT -> M.lookupGT k xs

lookupLE :: Ord k => k -> NonEmptyMap k v -> Maybe (k, v)
lookupLE a (NonEmptyMap k v xs) = case compare a k of
  LT -> Nothing
  EQ -> Just (k, v)
  GT -> M.lookupLT k xs <|> Just (k, v)

lookupGE :: Ord k => k -> NonEmptyMap k v -> Maybe (k, v)
lookupGE a (NonEmptyMap k v xs) = case compare a k of
  LT -> Just (k, v)
  EQ -> Just (k, v)
  GT -> M.lookupGT k xs

size :: NonEmptyMap k v -> Int
size (NonEmptyMap _ _ xs) = 1 + M.size xs

union :: (Ord k, Semigroup v)
  => NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
union (NonEmptyMap xk xv xs) (NonEmptyMap yk yv ys) = case compare xk yk of
  EQ -> NonEmptyMap xk xv (MS.union xs ys)
  LT -> NonEmptyMap xk xv (MS.union xs (M.insert yk yv ys))
  GT -> NonEmptyMap yk yv (MS.union (M.insert xk xv xs) ys)


unionAppend :: (Ord k, Semigroup v)
  => NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
unionAppend = unionWith (<>)

unionWith :: (Ord k, Semigroup v)
  => (v -> v -> v) -> NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
unionWith f (NonEmptyMap xk xv xs) (NonEmptyMap yk yv ys) = case compare xk yk of
  EQ -> NonEmptyMap xk (f xv yv) (MS.unionWith f xs ys)
  LT -> NonEmptyMap xk xv (MS.unionWith f xs (M.insertWith f yk yv ys))
  GT -> NonEmptyMap yk yv (MS.unionWith f (M.insertWith f xk xv xs) ys)

unionWithKey :: (Ord k, Semigroup v)
  => (k -> v -> v -> v) -> NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
unionWithKey f (NonEmptyMap xk xv xs) (NonEmptyMap yk yv ys) = case compare xk yk of
  EQ -> NonEmptyMap xk (f xk xv yv) (MS.unionWithKey f xs ys)
  LT -> NonEmptyMap xk xv (MS.unionWithKey f xs (M.insertWithKey f yk yv ys))
  GT -> NonEmptyMap yk yv (MS.unionWithKey f (M.insertWithKey f xk xv xs) ys)


foldr1 :: (v -> v -> v) -> NonEmptyMap k v -> v
foldr1 f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> f v (M.foldr f y m')

foldMap1 :: Semigroup m => (v -> m) -> NonEmptyMap k v -> m
foldMap1 f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> f v
  Just (x,m') -> f v <> M.foldr (\c d -> f c <> d) (f x) m'

foldMapWithKey :: Semigroup m => (k -> v -> m) -> NonEmptyMap k v -> m
foldMapWithKey f (NonEmptyMap k v m) = case M.maxView m of
  Nothing -> f k v
  Just (x,m') -> f k v <> M.foldrWithKey (\k' c d -> f k' c <> d) (f k x) m'

-- | __NOTE__: This differs from 'Semigroup' instance for normal maps!
instance (Ord k, Semigroup v) => Semigroup (NonEmptyMap k v) where
  (<>) = unionAppend

-- | Traverses elements in order ascending keys
instance Foldable (NonEmptyMap k) where
  fold (NonEmptyMap _ v m) = v <> F.fold m
  foldMap f (NonEmptyMap _ v m) = f v <> foldMap f m
  foldl f a (NonEmptyMap _ v m) = M.foldl f (f a v) m
  foldr f a (NonEmptyMap _ v m) = case M.maxView m of
    Nothing -> f v a
    Just (y,m') -> f v (M.foldr f (f y a) m')
  foldl1 f (NonEmptyMap _ v m) = M.foldl f v m
  foldr1 = foldr1
  length = size
  null _ = False
  minimum = foldl1' min
  maximum = foldl1' max

-- | Traverses elements in order ascending keys
instance Traversable (NonEmptyMap k) where
  traverse f (NonEmptyMap k v m) = NonEmptyMap k <$> f v <*> traverse f m
  sequenceA (NonEmptyMap k v m) = NonEmptyMap k <$> v <*> sequenceA m

instance Foldable1 (NonEmptyMap k) where
  toNonEmpty = elems
  fold1 = foldr1 (<>)
  foldMap1 = foldMap1

instance Traversable1 (NonEmptyMap k) where
  traverse1 f = fmap fromDistinctAscList . (traverse1 . traverse1) f . toNonEmpty
  sequence1 = fmap fromDistinctAscList . traverse1 sequence1 . toNonEmpty

elems :: NonEmptyMap k v -> NonEmpty v
elems (NonEmptyMap _ v m) = v :| M.elems m

keys :: NonEmptyMap k v -> NonEmpty k
keys (NonEmptyMap k _ m) = k :| M.keys m

keysSet :: NonEmptyMap k v -> NonEmptySet k
keysSet (NonEmptyMap k _ m) = NonEmptySet k (M.keysSet m)

foldl1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldl1' f (NonEmptyMap _ v m) = M.foldl' f v m

foldr1' :: (v -> v -> v) -> NonEmptyMap k v -> v
foldr1' f (NonEmptyMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> let !k = M.foldr' f y m' in f v k

mapWithKey :: (k -> v -> w) -> NonEmptyMap k v -> NonEmptyMap k w
mapWithKey f (NonEmptyMap k v m) = NonEmptyMap k (f k v) (M.mapWithKey f m)

-- assocs, toAscList
toNonEmpty :: NonEmptyMap k v -> NonEmpty (k,v)
toNonEmpty (NonEmptyMap k v m) = (k,v) :| M.toList m

-- maybe we can try to make this not require Ord by doing things manually
toMap :: Ord k => NonEmptyMap k v -> M.Map k v
toMap (NonEmptyMap k v m) = M.insert k v m

nonEmptyMap :: M.Map k v -> Maybe (NonEmptyMap k v)
nonEmptyMap m = (\((k, v), m') -> NonEmptyMap k v m') <$> M.minViewWithKey m

insertMap :: Ord k => k -> v -> M.Map k v -> NonEmptyMap k v
insertMap k v = maybe (singleton k v) (insert k v) . nonEmptyMap

insertMapWith :: Ord k => (v -> v -> v) -> k -> v -> M.Map k v -> NonEmptyMap k v
insertMapWith f k v = maybe (singleton k v) (insertWith f k v) . nonEmptyMap

insertMapWithKey :: Ord k => (k -> v -> v -> v) -> k -> v -> M.Map k v -> NonEmptyMap k v
insertMapWithKey f k v = maybe (singleton k v) (insertWithKey f k v) . nonEmptyMap

-- this could be implemented using insertWith, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
insert
    :: Ord k
    => k
    -> v
    -> NonEmptyMap k v
    -> NonEmptyMap k v
insert k v (NonEmptyMap k0 v0 m) = case compare k k0 of
  LT -> NonEmptyMap k  v  $ M.insert k0 v0 m
  EQ -> NonEmptyMap k  v  m
  GT -> NonEmptyMap k0 v0 $ M.insert k v m

-- this could be implemented using insertWithKey, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
insertWith
    :: Ord k
    => (v -> v -> v)
    -> k
    -> v
    -> NonEmptyMap k v
    -> NonEmptyMap k v
insertWith f k v (NonEmptyMap k0 v0 m) = case compare k k0 of
  LT -> NonEmptyMap k  v        $ M.insertWith f k0 v0 m
  EQ -> NonEmptyMap k  (f v v0) m
  GT -> NonEmptyMap k0 v0       $ M.insertWith f k  v  m

insertWithKey
    :: Ord k
    => (k -> v -> v -> v)
    -> k
    -> v
    -> NonEmptyMap k v
    -> NonEmptyMap k v
insertWithKey f k v (NonEmptyMap k0 v0 m) = case compare k k0 of
  LT -> NonEmptyMap k  v          $ M.insertWithKey f k0 v0 m
  EQ -> NonEmptyMap k  (f k v v0) m
  GT -> NonEmptyMap k0 v0         $ M.insertWithKey f k  v  m

insertLookupWithKey
    :: Ord k
    => (k -> v -> v -> v)
    -> k
    -> v
    -> NonEmptyMap k v
    -> (Maybe v, NonEmptyMap k v)
insertLookupWithKey f k v (NonEmptyMap k0 v0 m) = case compare k k0 of
  LT -> (Nothing, NonEmptyMap k  v          $ M.insertWithKey f k0 v0 m)
  EQ -> (Just v , NonEmptyMap k  (f k v v0) m                          )
  GT -> NonEmptyMap k0 v0 <$> M.insertLookupWithKey f k  v m

-- this could be implemented using fromListWith, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
fromList :: Ord k => NonEmpty (k, v) -> NonEmptyMap k v
fromList ((k, v) :| xs) = maybe (singleton k v) (insertWith (const id) k v)
                        . nonEmptyMap
                        $ M.fromList xs

fromListWith :: Ord k => (v -> v -> v) -> NonEmpty (k, v) -> NonEmptyMap k v
fromListWith f = fromListWithKey (const f)

fromListWithKey :: Ord k => (k -> v -> v -> v) -> NonEmpty (k, v) -> NonEmptyMap k v
fromListWithKey f ((k, v) :| xs) = maybe (singleton k v) (insertWith (flip (f k)) k v)
                                 . nonEmptyMap
                                 $ M.fromListWithKey f xs

fromAscList :: Eq k => NonEmpty (k, v) -> NonEmptyMap k v
fromAscList ((k, v) :| xs) = NonEmptyMap k (NE.last (v :| fmap snd xs0))
                           $ M.fromAscList xs1
  where
    (xs0, xs1) = span ((== k) . fst) xs

fromAscListWith :: Eq k => (v -> v -> v) -> NonEmpty (k, v) -> NonEmptyMap k v
fromAscListWith f = fromAscListWithKey (const f)

fromAscListWithKey :: Eq k => (k -> v -> v -> v) -> NonEmpty (k, v) -> NonEmptyMap k v
fromAscListWithKey f ((k, v) :| xs) = NonEmptyMap k vFinal
                                    $ M.fromAscListWithKey f xs1
  where
    (xs0, xs1) = span ((== k) . fst) xs
    vFinal     = F.foldl1 (f k) (v :| fmap snd xs0)

fromDistinctAscList :: NonEmpty (k, v) -> NonEmptyMap k v
fromDistinctAscList ((k, v) :| xs) = NonEmptyMap k v
                                   $ M.fromDistinctAscList xs


delete :: Ord k => k -> NonEmptyMap k v -> M.Map k v
delete k m@(NonEmptyMap k0 _ xs) = case compare k k0 of
  LT -> toMap m
  EQ -> xs
  GT -> toMap m

adjust :: Ord k => (v -> v) -> k -> NonEmptyMap k v -> NonEmptyMap k v
adjust f = adjustWithKey (const f)

adjustWithKey :: Ord k => (k -> v -> v) -> k -> NonEmptyMap k v -> NonEmptyMap k v
adjustWithKey f k m@(NonEmptyMap k0 v xs) = case compare k k0 of
  LT -> m
  EQ -> NonEmptyMap k0 (f k v) xs
  GT -> NonEmptyMap k0 v . M.adjustWithKey f k $ xs

update :: Ord k => (v -> Maybe v) -> k -> NonEmptyMap k v -> M.Map k v
update f = updateWithKey (const f)

updateWithKey :: Ord k => (k -> v -> Maybe v) -> k -> NonEmptyMap k v -> M.Map k v
updateWithKey f k m@(NonEmptyMap k0 v xs) = case compare k k0 of
  LT -> toMap m
  EQ -> maybe xs (flip (M.insert k) xs) . f k $ v
  GT -> M.insert k0 v . M.updateWithKey f k $ xs

updateLookupWithKey :: Ord k => (k -> v -> Maybe v) -> k -> NonEmptyMap k v -> (Maybe v, M.Map k v)
updateLookupWithKey f k m@(NonEmptyMap k0 v xs) = case compare k k0 of
  LT -> (Nothing, toMap m)
  EQ -> (Just v , maybe xs (flip (M.insert k) xs) . f k $ v)
  GT -> fmap (M.insert k0 v) . M.updateLookupWithKey f k $ xs

alter :: Ord k => (Maybe v -> Maybe v) -> k -> NonEmptyMap k v -> M.Map k v
alter f k m@(NonEmptyMap k0 v xs) = case compare k k0 of
  LT -> ($ toMap m) . maybe id (M.insert k) $ f Nothing
  EQ -> ($ xs     ) . maybe id (M.insert k) $ f (Just v)
  GT -> M.insert k0 v . M.alter f k $ xs

alterF
    :: (Ord k, Functor f)
    => (Maybe v -> f (Maybe v))
    -> k
    -> NonEmptyMap k v
    -> f (M.Map k v)
alterF f k m@(NonEmptyMap k0 v xs) = case compare k k0 of
  LT -> ($ toMap m) . maybe id (M.insert k) <$> f Nothing
  EQ -> ($ xs     ) . maybe id (M.insert k) <$> f (Just v)
  GT -> M.insert k0 v <$> M.alterF f k xs

map :: (a -> b) -> NonEmptyMap k a -> NonEmptyMap k b
map = fmap

traverseWithKey
  :: Applicative t => (k -> a -> t b) -> NonEmptyMap k a -> t (NonEmptyMap k b)
traverseWithKey f (NonEmptyMap k v xs) = NonEmptyMap k <$> f k v
                                                       <*> M.traverseWithKey f xs

-- TODO: traverseWithKey1

-- Requires Ord k
traverseMaybeWithKey
  :: (Ord k, Applicative t) => (k -> a -> t (Maybe b)) -> NonEmptyMap k a -> t (M.Map k b)
traverseMaybeWithKey f (NonEmptyMap k v xs) =
    combine <$> f k v <*> M.traverseMaybeWithKey f xs
  where
    combine Nothing   = id
    combine (Just v') = M.insert k v'

mapAccum
  :: (a -> b -> (a, c))
  -> a
  -> NonEmptyMap k b
  -> (a, NonEmptyMap k c)
mapAccum f = mapAccumWithKey (\x _ -> f x)

mapAccumWithKey
  :: (a -> k -> b -> (a, c))
  -> a
  -> NonEmptyMap k b
  -> (a, NonEmptyMap k c)
mapAccumWithKey f z0 (NonEmptyMap k v xs) = (z2, NonEmptyMap k v' xs')
  where
    (z1, v' ) = f z0 k v
    (z2, xs') = M.mapAccumWithKey f z1 xs

-- Result can be smaller, but not empty
mapKeys :: Ord k2 => (k1 -> k2) -> NonEmptyMap k1 v -> NonEmptyMap k2 v
mapKeys f (NonEmptyMap k v xs) = maybe (singleton (f k) v) (insertWith (const id) (f k) v)
                               . nonEmptyMap
                               $ M.mapKeys f xs

-- Result can be smaller, but not empty
mapKeysWith
  :: Ord k2 => (v -> v -> v) -> (k1 -> k2) -> NonEmptyMap k1 v -> NonEmptyMap k2 v
mapKeysWith c f (NonEmptyMap k v xs) =
     maybe (singleton (f k) v) (insertWith (flip c) (f k) v)
   . nonEmptyMap
   $ M.mapKeysWith c f xs

-- Result can be smaller, but not empty
mapKeysMonotonic
  :: (k1 -> k2) -> NonEmptyMap k1 v -> NonEmptyMap k2 v
mapKeysMonotonic f (NonEmptyMap k v xs) = NonEmptyMap (f k) v
                                        $ M.mapKeysMonotonic f xs

-- Requires Ord k
filter :: Ord k => (a -> Bool) -> NonEmptyMap k a -> M.Map k a
filter f (NonEmptyMap k v xs)
  | f v       = M.insert k v . M.filter f $ xs
  | otherwise = M.filter f xs

-- Requires Ord k
filterWithKey :: Ord k => (k -> a -> Bool) -> NonEmptyMap k a -> M.Map k a
filterWithKey f (NonEmptyMap k v xs)
  | f k v     = M.insert k v . M.filterWithKey f $ xs
  | otherwise = M.filterWithKey f xs

restrictKeys :: Ord k => NonEmptyMap k a -> S.Set k -> M.Map k a
restrictKeys (NonEmptyMap k v xs) ks
  | S.member k ks = M.insert k v . M.restrictKeys xs $ ks
  | otherwise     = M.restrictKeys xs ks

withoutKeys :: Ord k => NonEmptyMap k a -> S.Set k -> M.Map k a
withoutKeys (NonEmptyMap k v xs) ks
  | S.member k ks = M.restrictKeys xs ks
  | otherwise     = M.insert k v . M.restrictKeys xs $ ks

-- Requires Ord k
partition :: Ord k => (a -> Bool) -> NonEmptyMap k a -> (M.Map k a, M.Map k a)
partition f = partitionWithKey (const f)

-- Requires Ord k
partitionWithKey :: Ord k => (k -> a -> Bool) -> NonEmptyMap k a -> (M.Map k a, M.Map k a)
partitionWithKey f (NonEmptyMap k v xs)
    | f k v     = (M.insert k v ys, zs             )
    | otherwise = (ys             , M.insert k v zs)
  where
    (ys, zs) = M.partitionWithKey f xs

takeWhileAntitone :: Ord k => (k -> Bool) -> NonEmptyMap k v -> M.Map k v
takeWhileAntitone f (NonEmptyMap k v xs)
  | f k       = M.insert k v . M.takeWhileAntitone f $ xs
  | otherwise = M.empty

dropWhileAntitone :: Ord k => (k -> Bool) -> NonEmptyMap k v -> M.Map k v
dropWhileAntitone f m@(NonEmptyMap k _ xs)
  | f k       = M.dropWhileAntitone f $ xs
  | otherwise = toMap m

spanAntitone :: Ord k => (k -> Bool) -> NonEmptyMap k v -> (M.Map k v, M.Map k v)
spanAntitone f m@(NonEmptyMap k v xs)
  | f k       = first (M.insert k v) . M.spanAntitone f $ xs
  | otherwise = (M.empty, toMap m)

mapMaybe :: Ord k => (a -> Maybe b) -> NonEmptyMap k a -> M.Map k b
mapMaybe f = mapMaybeWithKey (const f)

-- Requires Ord k
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> NonEmptyMap k a -> M.Map k b
mapMaybeWithKey f (NonEmptyMap k v xs) = ($ M.mapMaybeWithKey f xs)
                                       . maybe id (M.insert k)
                                       $ f k v

-- Requires Ord k
mapEither :: Ord k => (a -> Either b c) -> NonEmptyMap k a -> (M.Map k b, M.Map k c)
mapEither f = mapEitherWithKey (const f)

-- Requires Ord k
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> NonEmptyMap k a -> (M.Map k b, M.Map k c)
mapEitherWithKey f (NonEmptyMap k v xs) = case f k v of
    Left  v' -> (M.insert k v' ys, zs)
    Right v' -> (ys, M.insert k v' zs)
  where
    (ys, zs) = M.mapEitherWithKey f xs

split :: Ord k => k -> NonEmptyMap k v -> (M.Map k v, M.Map k v)
split k m@(NonEmptyMap k0 v xs) = case compare k k0 of
    LT -> (M.empty, toMap m)
    EQ -> (M.empty, xs     )
    GT -> first (M.insert k0 v) . M.split k $ xs

splitLookup :: Ord k => k -> NonEmptyMap k v -> (M.Map k v, Maybe v, M.Map k v)
splitLookup k m@(NonEmptyMap k0 v xs) = case compare k k0 of
    LT -> (M.empty, Nothing, toMap m)
    EQ -> (M.empty, Just v , xs     )
    GT -> let (ys, x, zs) = M.splitLookup k xs
          in  (M.insert k0 v ys, x, zs)

isSubmapOf :: (Ord k, Eq a) => NonEmptyMap k a -> NonEmptyMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- performance benefit: can short-circuit, skip an insert
isSubmapOfBy :: Ord k => (a -> b -> Bool) -> NonEmptyMap k a -> NonEmptyMap k b -> Bool
isSubmapOfBy f (toMap->m0) (NonEmptyMap k v m) = kvSub
                                              && M.isSubmapOfBy f m0 m
  where
    kvSub = case M.lookup k m0 of
      Just v0 -> f v0 v
      Nothing -> False

-- is there a better way to do this?
isProperSubmapOf :: (Ord k, Eq a) => NonEmptyMap k a -> NonEmptyMap k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)


-- is there a better way to do this?
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> NonEmptyMap k a -> NonEmptyMap k b -> Bool
isProperSubmapOfBy f m1 m2 = M.isProperSubmapOfBy f (toMap m1) (toMap m2)

lookupMin :: NonEmptyMap k v -> (k, v)
lookupMin (NonEmptyMap k v _) = (k, v)

lookupMax :: NonEmptyMap k v -> (k, v)
lookupMax (NonEmptyMap k v m) = fromMaybe (k, v) . M.lookupMax $ m

deleteMin :: NonEmptyMap k v -> M.Map k v
deleteMin (NonEmptyMap _ _ m) = m

-- requires Ord
deleteMax :: Ord k => NonEmptyMap k v -> M.Map k v
deleteMax (NonEmptyMap k v m) = M.insert k v . M.deleteMax $ m

updateMin :: Ord k => (v -> Maybe v) -> NonEmptyMap k v -> M.Map k v
updateMin f = updateMinWithKey (const f)

updateMinWithKey :: Ord k => (k -> v -> Maybe v) -> NonEmptyMap k v -> M.Map k v
updateMinWithKey f (NonEmptyMap k v m) = ($ m) . maybe id (M.insert k) $ f k v

updateMax :: Ord k => (v -> Maybe v) -> NonEmptyMap k v -> M.Map k v
updateMax f = updateMaxWithKey (const f)

updateMaxWithKey :: Ord k => (k -> v -> Maybe v) -> NonEmptyMap k v -> M.Map k v
updateMaxWithKey f (NonEmptyMap k v m) = M.insert k v . M.updateMaxWithKey f $ m

minView :: NonEmptyMap k v -> (v, M.Map k v)
minView = first snd . minViewWithKey

minViewWithKey :: NonEmptyMap k v -> ((k, v), M.Map k v)
minViewWithKey (NonEmptyMap k v m) = ((k, v), m)

-- requires Ord
maxView :: Ord k => NonEmptyMap k v -> (v, M.Map k v)
maxView = first snd . maxViewWithKey

-- requires Ord
maxViewWithKey :: Ord k => NonEmptyMap k v -> ((k, v), M.Map k v)
maxViewWithKey (NonEmptyMap k v m) = maybe ((k, v), M.empty) (second (M.insert k v))
                                   $ M.maxViewWithKey m

valid :: Ord k => NonEmptyMap k v -> Bool
valid (NonEmptyMap k _ m) = all (k <) (M.keys m) && M.valid m
