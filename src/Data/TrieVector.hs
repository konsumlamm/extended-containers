module Data.TrieVector
( Vector
, empty
, singleton
, fromList
, size
, snoc
, lookup
, update
, adjust
, map
) where

import Data.Bits
import Data.Foldable (foldl', toList)
import Data.Vector((!), (//))
import qualified Data.Vector as V
import Prelude hiding (lookup, map)

-- An Array Mapped Trie.
data Tree a = Internal !(V.Vector (Tree a))
            | Leaf !(V.Vector a)
            deriving Show
data Vector a = Empty
              | Root !Int !Int !(Tree a)  -- Root size height tree

bits :: Int
bits = 4
{-# INLINE bits #-}

mask :: Int
mask = (1 `shiftL` bits) - 1
{-# INLINE mask #-}


instance Show a => Show (Vector a) where
    show v = "fromList " ++ show (toList v)

instance Foldable Vector where
    foldMap _ Empty = mempty
    foldMap f (Root _ _ t) = foldMapTree t
      where
        foldMapTree (Internal v) = foldMap foldMapTree v
        foldMapTree (Leaf v) = foldMap f v

    length = size
    {-# INLINE length #-}

instance Functor Vector where
    fmap = map
    {-# INLINE fmap #-}

instance Traversable Vector where
    traverse _ Empty = pure Empty
    traverse f (Root s h t) = Root s h <$> traverseTree t
      where
        traverseTree (Internal v) = Internal <$> traverse traverseTree v
        traverseTree (Leaf v) = Leaf <$> traverse f v

empty :: Vector a
empty = Empty
{-# INLINE empty #-}

singleton :: a -> Vector a
singleton x = Root 1 1 (Leaf $ V.singleton x)

fromList :: [a] -> Vector a
fromList = foldl' snoc empty

size :: Vector a -> Int
size Empty = 0
size (Root s _ _) = s

snoc :: Vector a -> a -> Vector a
snoc Empty x = singleton x
snoc (Root s h t) x
    | s == 1 `shiftL` (bits * h) = Root (s + 1) (h + 1) (Internal $ V.fromList [t, newPath h])
    | otherwise = Root (s + 1) h (insertTree (bits * (h - 1)) t)
  where
    newPath 1 = Leaf $ V.singleton x
    newPath h = Internal $ V.singleton (newPath (h - 1)) 

    insertTree sh (Internal v)
        | index < V.length v = Internal $ v // [(index, insertTree (sh - bits) (v ! index))]
        | otherwise = Internal $ V.snoc v (newPath (sh `div` bits))
      where
        index = s `shiftR` sh .&. mask
    insertTree _ (Leaf v) = Leaf $ V.snoc v x 

lookup :: Int -> Vector a -> Maybe a
lookup _ Empty = Nothing
lookup i (Root s h t)
    | i < 0 || i >= s = Nothing
    | otherwise = Just $ lookupTree (bits * (h - 1)) t
  where
    lookupTree sh (Internal v) = lookupTree (sh - bits) (v ! (i `shiftR` sh .&. mask))
    lookupTree _ (Leaf v) = v ! (i .&. mask)

update :: Int -> a -> Vector a -> Vector a
update i x = adjust i (const x)

adjust :: Int -> (a -> a) -> Vector a -> Vector a
adjust _ _ Empty = Empty
adjust i f (Root s h t)
    | i < 0 || i >= s = Root s h t
    | otherwise = Root s h (adjustTree (bits * (h - 1)) t)
  where
    adjustTree sh (Internal v) =
        let index = i `shiftR` sh .&. mask
        in Internal $ v // [(index, adjustTree (sh - bits) (v ! index))]
    adjustTree _ (Leaf v) =
        let index = i .&. mask
        in Leaf $ v // [(index, f (v ! index))]

map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root s h t) = Root s h (mapTree t)
  where
    mapTree (Internal v) = Internal (fmap mapTree v)
    mapTree (Leaf v) = Leaf (fmap f v)
