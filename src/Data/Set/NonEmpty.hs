module Data.Set.NonEmpty
  ( NonEmptySet
  , singleton
  , toNonEmpty
  , fromNonEmpty
  ) where

import Data.Foldable
import Data.List.NonEmpty
import qualified Data.Set as S

-- | A non-empty set.
data NonEmptySet a = NonEmptySet !a !(S.Set a)
  deriving (Eq,Ord)

-- The internal invariant for a NonEmptySet is that the first
-- element in the NonEmptySet data constructor must be less
-- than everything in the Set that is the second argument.

-- | Create a non-empty set with a single element.
singleton :: a -> NonEmptySet a
singleton x = NonEmptySet x S.empty

-- | Convert a non-empty set to a non-empty list.
toNonEmpty :: NonEmptySet a -> NonEmpty a
toNonEmpty (NonEmptySet x xs) = x :| S.toList xs

-- | Create a non-empty set from a non-empty list.
fromNonEmpty :: Ord a => NonEmpty a -> NonEmptySet a
fromNonEmpty (x :| xs) = case S.minView s of
  Nothing -> NonEmptySet x S.empty
  Just (m,s') -> case compare x m of
    EQ -> NonEmptySet m s'
    GT -> NonEmptySet m (S.insert x s')
    LT -> NonEmptySet x s
  where
  s = S.fromList xs

instance Show a => Show (NonEmptySet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromNonEmpty " . shows (toNonEmpty xs)

instance Foldable NonEmptySet where
  fold (NonEmptySet a s) = a <> fold s
  foldMap f (NonEmptySet a s) = f a <> foldMap f s
  foldl1 f (NonEmptySet a s) = S.foldl f a s
  foldr1 f (NonEmptySet a s) = case S.maxView s of
    Nothing -> a
    Just (m,s') -> f a (S.foldr f m s')
  minimum (NonEmptySet a _) = a
  maximum (NonEmptySet a s) = case S.lookupMax s of
    Nothing -> a
    Just m -> m

instance Ord a => Semigroup (NonEmptySet a) where
  NonEmptySet x xs <> NonEmptySet y ys = case compare x y of
    EQ -> NonEmptySet x (xs <> ys)
    LT -> NonEmptySet x (xs <> S.insert y ys)
    GT -> NonEmptySet y (S.insert x xs <> ys)
