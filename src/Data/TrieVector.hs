{-# LANGUAGE TypeFamilies #-}

-- |
-- = Finite vectors
--
-- The @'Vector' a@ type represents a finite vector of elements of type @a@.
-- A 'Vector' is strict in its spine.
--
-- The class instances are based on those for lists.
--
-- This module should be imported qualified, to avoid name clashes with the 'Prelude'.
--
-- > import Qualified Data.TrieVector as Vector
--
-- == Performance
--
-- The running time complexities are given, with /n/ referring the the number of elements in the vector.
-- A 'Vector' is particularily efficient for applications that require a lot of indexing and updates.
-- The given running times are worst case. All logarithms are base 16.
--
-- == Warning
--
-- The length of a 'Vector' must not exceed @'maxBound' :: 'Int'@.
-- Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the vector is undefined.
--
-- == Implementation
--
-- The implementation of 'Vector' uses array mapped tries.

module Data.TrieVector
( Vector
-- * Construction
, empty
, singleton
, fromList

, snoc
, append
, last

, lookup
, (!?)
, update
, adjust

, map

, mapWithIndex
, foldMapWithIndex
, foldlWithIndex
, foldrWithIndex
, foldlWithIndex'
, foldrWithIndex'
, traverseWithIndex
, indexed

, unfoldr
, unfoldl
-- * Zipping/Unzipping
, zip
, zipWith
, zip3
, zipWith3
, unzip
, unzip3
) where

import Control.Applicative (Alternative)
import qualified Control.Applicative as Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Zip (MonadZip(..))

import Data.Bits
import Data.Foldable (foldl', length, toList)
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty(..), (!!), (<|))
import qualified Data.List.NonEmpty as L
import Data.Traversable (mapAccumL)
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
import Prelude hiding ((!!), last, lookup, map, tail, unzip, unzip3, zip, zipWith, zip3, zipWith3)
import Text.Read (readPrec)

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Data.Traversable.Utils (traverseAccumL)

data Tree a = Internal !(V.Vector (Tree a))
            | Leaf !(V.Vector a)
-- | An Array Mapped Trie.
data Vector a = Empty
              | Root {-# UNPACK #-} !Int  -- size
                     {-# UNPACK #-} !Int  -- offset
                     {-# UNPACK #-} !Int  -- height
                     !(Tree a)  -- tree
                     !(NonEmpty a)  -- tail (reversed)

-- The number of bits used per level.
bits :: Int
bits = 4
{-# INLINE bits #-}

-- The mask used to extract the index into the array.
mask :: Int
mask = (1 `shiftL` bits) - 1


instance Show1 Vector where
    liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance Show a => Show (Vector a) where
    showsPrec = showsPrec1
    {-# INLINE showsPrec #-}

instance Read1 Vector where
    liftReadPrec rp rl = readData $ readUnaryWith (liftReadPrec rp rl) "fromList" fromList

instance Read a => Read (Vector a) where
    readPrec = readPrec1

instance Eq1 Vector where
    liftEq f v1 v2 = length v1 == length v2 && liftEq f (toList v1) (toList v2)

instance Eq a => Eq (Vector a) where
    (==) = eq1
    {-# INLINE (==) #-}

instance Ord1 Vector where
    liftCompare f v1 v2 = liftCompare f (toList v1) (toList v2)

instance Ord a => Ord (Vector a) where
    compare = compare1
    {-# INLINE compare #-}

instance Semigroup (Vector a) where
    (<>) = append
    {-# INLINE (<>) #-}

instance Monoid (Vector a) where
    mempty = empty
    {-# INLINE mempty #-}

instance Foldable Vector where
    foldr _ acc Empty = acc
    foldr f acc (Root _ _ _ tree tail) = foldrTree tree (foldr f acc (L.reverse tail))
      where
        foldrTree (Internal v) acc = foldr foldrTree acc v
        foldrTree (Leaf v) acc = foldr f acc v

    length Empty = 0
    length (Root s _ _ _ _) = s

instance Functor Vector where
    fmap = map
    {-# INLINE fmap #-}

instance Traversable Vector where
    traverse _ Empty = pure Empty
    traverse f (Root s offset h tree tail) =
        Root s offset h <$> traverseTree tree <*> (L.reverse <$> traverse f (L.reverse tail))
      where
        traverseTree (Internal v) = Internal <$> traverse traverseTree v
        traverseTree (Leaf v) = Leaf <$> traverse f v

instance IsList (Vector a) where
    type Item (Vector a) = a
    fromList = fromList
    toList = toList


instance Applicative Vector where
    pure = singleton
    {-# INLINE pure #-}

    fs <*> xs = foldl' (\acc f -> append acc (map f xs)) empty fs

instance Monad Vector where
    xs >>= f = foldl' (\acc x -> append acc (f x)) empty xs

instance Alternative Vector where
    empty = empty
    {-# INLINE empty #-}

    (<|>) = append
    {-# INLINE (<|>) #-}

instance MonadPlus Vector

instance MonadFail Vector where
    fail _ = empty
    {-# INLINE fail #-}

instance MonadZip Vector where
    mzip = zip
    {-# INLINE mzip #-}

    mzipWith = zipWith
    {-# INLINE mzipWith #-}

    munzip = unzip
    {-# INLINE munzip #-}


-- | /O(1)/. The empty vector.
empty :: Vector a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A vector with a single element.
singleton :: a -> Vector a
singleton x = Root 1 0 0 (Leaf V.empty) (x :| [])

-- | /O(n * log n)/. Create a new vector from a list.
fromList :: [a] -> Vector a
fromList = foldl' snoc empty

-- | /O(log n)/. Add an element to the right end of the vector.
snoc :: Vector a -> a -> Vector a
snoc Empty x = singleton x
snoc (Root s offset h tree tail) x
    | s .&. mask /= 0 = Root (s + 1) offset h tree (x <| tail)
    | offset == 0 = Root (s + 1) s (h + 1) (Leaf $ V.fromList (toList $ L.reverse tail)) (x :| [])
    | offset == 1 `shiftL` (bits * h) = Root (s + 1) s (h + 1) (Internal $ V.fromList [tree, newPath h]) (x :| [])
    | otherwise = Root (s + 1) s h (insertTail (bits * (h - 1)) tree) (x :| [])
  where
    newPath 1 = Leaf $ V.fromList (toList $ L.reverse tail)
    newPath h = Internal $ V.singleton (newPath (h - 1))

    insertTail sh (Internal v)
        | index < V.length v = Internal $ V.modify (\v -> M.modify v (insertTail (sh - bits)) index) v
        | otherwise = Internal $ V.snoc v (newPath (sh `div` bits))
      where
        index = offset `shiftR` sh .&. mask
    insertTail _ (Leaf _) = Leaf $ V.fromList (toList $ L.reverse tail)

-- | /O(1)/. The last element in the vector.
last :: Vector a -> Maybe a
last Empty = Nothing
last (Root _ _ _ _ (x :| _)) = Just x

-- | /O(log n)/. The element at the index or 'Nothing' if the index is out of range.
lookup :: Int -> Vector a -> Maybe a
lookup _ Empty = Nothing
lookup i (Root s offset h tree tail)
    | i < 0 || i >= s = Nothing
    | i < offset = Just $ lookupTree (bits * (h - 1)) tree
    | otherwise = Just $ tail !! (s - i - 1)
  where
    lookupTree sh (Internal v) = lookupTree (sh - bits) (v ! (i `shiftR` sh .&. mask))
    lookupTree _ (Leaf v) = v ! (i .&. mask)

-- | /O(log n)/. Flipped version of 'lookup'.
(!?) :: Vector a -> Int -> Maybe a
(!?) = flip lookup

-- | /O(log n)/. Update the element at the index with a new element.
-- Returns the original vector if the index is out of range.
update :: Int -> a -> Vector a -> Vector a
update i x = adjust i (const x)
{-# INLINE update #-}

-- | /O(log n)/. Adjust the element at the index by applying the function to it.
-- Returns the original vector if the index is out of range.
adjust :: Int -> (a -> a) -> Vector a -> Vector a
adjust _ _ Empty = Empty
adjust i f root@(Root s offset h tree tail)
    | i < 0 || i >= s = root
    | i < offset = Root s offset h (adjustTree (bits * (h - 1)) tree) tail
    | otherwise = let (l, x : r) = L.splitAt (s - i - 1) tail in Root s offset h tree (L.fromList $ l ++ (f x : r))
  where
    adjustTree sh (Internal v) =
        let index = i `shiftR` sh .&. mask
        in Internal $ V.modify (\v -> M.modify v (adjustTree (sh - bits)) index) v
    adjustTree _ (Leaf v) =
        let index = i .&. mask
        in Leaf $ V.modify (\v -> M.modify v f index) v

-- | Concatenate two vectors.
append :: Vector a -> Vector a -> Vector a
append Empty v = v
append v Empty = v
append v1 v2 = foldl' snoc v1 v2

-- | /O(n)/. Map a function over the vector.
map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root s offset h tree tail) = Root s offset h (mapTree tree) (fmap f tail)
  where
    mapTree (Internal v) = Internal (fmap mapTree v)
    mapTree (Leaf v) = Leaf (fmap f v)

-- | /O(n)/. Map a function that has access to the index of an element over the vector.
mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

-- | /O(n)/. Fold using the given monoid.
foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Vector a -> m
foldMapWithIndex f = foldrWithIndex (\i -> mappend . f i) mempty

-- | /O(n)/. Fold using the given left-associative function that has access to the index of an element.
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Vector a -> b
foldlWithIndex f z0 v = foldl (\g x i -> i `seq` f (g (i - 1)) i x) (const z0) v (length v - 1)

-- | /O(n)/. Fold using the given right-associative function that has access to the index of an element.
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Vector a -> b
foldrWithIndex f z0 v = foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z0) v 0

-- | /O(n)/. A strict version of 'foldlWithIndex'.
-- Each application of the function is evaluated before using the result in the next application.
foldlWithIndex' :: (b -> Int -> a -> b) -> b -> Vector a -> b
foldlWithIndex' f z0 v = foldrWithIndex f' id v z0
  where
    f' i x k z = k $! f z i x

-- | /O(n)/. A strict version of 'foldrWithIndex'.
-- Each application of the function is evaluated before using the result in the next application.
foldrWithIndex' :: (Int -> a -> b -> b) -> b -> Vector a -> b
foldrWithIndex' f z0 v = foldlWithIndex f' id v z0
  where
    f' k i x z = k $! f i x z

-- | /O(n)/. Traverse the vector with a function that has access to the index of an element.
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Vector a -> f (Vector b)
traverseWithIndex f = snd . traverseAccumL (\i x -> (i + 1, f i x)) 0

indexed :: Vector a -> Vector (Int, a)
indexed = mapWithIndex (,)
{-# INLINE indexed #-}

-- | /O(n * log n)/. Build a vector from left to right by repeatedly applying a function to a seed value.
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f = go empty
  where
    go v acc = case f acc of
        Nothing -> v
        Just (x, acc') -> go (snoc v x) acc'
{-# INLINE unfoldr #-}

-- | /O(n * log n)/. Build a vector from right to left by repeatedly applying a function to a seed value.
unfoldl :: (b -> Maybe (b, a)) -> b -> Vector a
unfoldl f acc = case f acc of
    Nothing -> empty
    Just (acc', x) -> snoc (unfoldl f acc') x

zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f v1 v2
    | length v1 >= length v2 = snd $ mapAccumL f' (toList v1) v2
    | otherwise = zipWith (flip f) v2 v1
  where
    f' [] _ = error "unreachable"
    f' (x : xs) y = (xs, f x y)

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f v1 v2 v3 = zipWith ($) (zipWith f v1 v2) v3

-- | /O(n)/.
unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip v = (map fst v, map snd v)

-- | /O(n)/.
unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 v = (map fst3 v, map snd3 v, map trd3 v)
  where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    trd3 (_, _, z) = z
