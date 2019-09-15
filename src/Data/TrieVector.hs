{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

-- | All logarithms are base 16.

module Data.TrieVector
( Vector
, empty
, singleton
, fromList
, snoc
, last
, lookup
, (!?)
, update
, adjust
, map
) where

import qualified Control.Applicative as Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fail (MonadFail(..))
import Data.Bits
import Data.Foldable (foldl', length, toList)
import Data.List.NonEmpty (NonEmpty(..), (!!), (<|))
import qualified Data.List.NonEmpty as L
import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified GHC.Exts as Exts
import Prelude hiding ((!!), last, lookup, map, tail)

data Tree a = Internal !(V.Vector (Tree a))
            | Leaf !(V.Vector a)
-- | An Array Mapped Trie.
data Vector a = Empty
              | Root !Int  -- size
                     !Int  -- offset
                     !Int  -- height
                     !(Tree a)  -- tree
                     !(NonEmpty a)  -- tail (reversed)

-- The number of bits used per level.
bits :: Int
bits = 4
{-# INLINE bits #-}

-- The mask used to extract the index into the array.
mask :: Int
mask = (1 `shiftL` bits) - 1


instance Show a => Show (Vector a) where
    show v = "fromList " ++ show (toList v)

instance Eq a => Eq (Vector a) where
    v1 == v2 = length v1 == length v2 && toList v1 == toList v2

instance Ord a => Ord (Vector a) where
    v1 `compare` v2 = toList v1 `compare` toList v2

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

instance Exts.IsList (Vector a) where
    type Item (Vector a) = a
    fromList = fromList
    toList = toList


instance Applicative Vector where
    pure = singleton
    {- INLINE pure #-}

    fs <*> xs = foldl' (\acc f -> append acc (map f xs)) empty fs

instance Monad Vector where
    xs >>= f = foldl' (\acc x -> append acc (f x)) empty xs

instance Applicative.Alternative Vector where
    empty = empty
    {-# INLINE empty #-}

    (<|>) = append
    {-# INLINE (<|>) #-}

instance MonadPlus Vector

instance MonadFail Vector where
    fail _ = empty
    {-# INLINE fail #-}


-- | /O(1)/. The empty vector.
empty :: Vector a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A vector with a single element.
singleton :: a -> Vector a
singleton x = Root 1 0 0 (Leaf V.empty) [x]

-- | /O(n * log n)/. Create a new vector from a list.
fromList :: [a] -> Vector a
fromList = foldl' snoc empty

-- | /O(log n)/. Add an element to the right end of the vector.
snoc :: Vector a -> a -> Vector a
snoc Empty x = singleton x
snoc (Root s offset h tree tail) x
    | s .&. mask /= 0 = Root (s + 1) offset h tree (x <| tail)
    | offset == 0 = Root (s + 1) s (h + 1) (Leaf $ V.fromList (toList $ L.reverse tail)) [x]
    | offset == 1 `shiftL` (bits * h) = Root (s + 1) s (h + 1) (Internal $ V.fromList [tree, newPath h]) [x]
    | otherwise = Root (s + 1) s h (insertTail (bits * (h - 1)) tree) [x]
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

-- | /O(log n)/. Update the element at the index with a new element. Returns the original vector if the index is out of range.
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

-- | /O(n)/. Map a function over the vector.
map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root s offset h tree tail) = Root s offset h (mapTree tree) (fmap f tail)
  where
    mapTree (Internal v) = Internal (fmap mapTree v)
    mapTree (Leaf v) = Leaf (fmap f v)

-- | Concatenate two vectors.
append :: Vector a -> Vector a -> Vector a
append Empty v = v
append v Empty = v
append v1 v2 = foldl' snoc v1 v2
