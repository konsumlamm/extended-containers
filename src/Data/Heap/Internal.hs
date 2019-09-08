module Data.Heap.Internal
( Heap(..)
-- * Construction
, empty
, singleton
-- ** From Lists
, fromList
, fromAscList
, fromDescList
-- * Insertion/Union
, insert
, union
, unions
-- * Traversal/Filter
, map
, mapMonotonic
, filter
, partition
-- * Query
, member
, size
-- * Min
, lookupMin
, findMin
, deleteMin
, deleteFindMin
, minView
-- * Conversion
-- ** To Lists
, toAscList
, toDescList
) where

import Prelude hiding (filter, map)
import Data.Foldable (foldl', toList)

-- A Leftist Heap.
type Size = Int
type Rank = Int
data Heap a = Leaf | Node !Size !Rank !a !(Heap a) !(Heap a)

node :: a -> Heap a -> Heap a -> Heap a
node x heap Leaf = Node (size heap + 1) 1 x heap Leaf
node x Leaf heap = Node (size heap + 1) 1 x heap Leaf
node x heap1@(Node s1 r1 _ _ _) heap2@(Node s2 r2 _ _ _)
    | r1 >= r2 = Node (s1 + s2) (r2 + 1) x heap1 heap2
    | otherwise = Node (s1 + s2) (r1 + 1) x heap2 heap1

errorEmpty :: String -> a
errorEmpty s = error $ "Heap." ++ s ++ ": empty heap"


instance Show a => Show (Heap a) where
    show heap = "fromList " ++ show (toList heap)

instance Ord a => Semigroup (Heap a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Ord a => Monoid (Heap a) where
    mempty = empty
    {-# INLINE mempty #-}

instance Foldable Heap where
    foldMap _ Leaf = mempty
    foldMap f (Node _ _ x left right) =
        f x <> foldMap f left <> foldMap f right

    null Leaf = True
    null (Node _  _ _ _ _) = False

    length = size
    {-# INLINE length #-}

    minimum = findMin
    {-# INLINE minimum #-}


-- | /O(1)/.
empty :: Heap a
empty = Leaf
{-# INLINE empty #-}

-- | /O(1)/.
singleton :: a -> Heap a
singleton x = Node 1 1 x Leaf Leaf
{-# INLINE singleton #-}

-- | /O(n)/.
fromList :: Ord a => [a] -> Heap a
fromList ls = fromList' (fmap singleton ls) []
  where
    fromList' [] [] = empty
    fromList' [x] [] = x
    fromList' (x1 : x2 : xs) ys = fromList' xs (union x1 x2 : ys)
    fromList' xs ys = fromList' (xs ++ reverse ys) []

-- | /O(n)/. The precondition is not checked.
fromAscList :: Ord a => [a] -> Heap a
fromAscList = foldr (\x acc -> node x acc empty) empty

-- | /O(n)/. The precondition is not checked.
fromDescList :: Ord a => [a] -> Heap a
fromDescList = foldl' (\acc x -> node x acc empty) empty

-- | /O(log n)/.
insert :: Ord a => a -> Heap a -> Heap a
insert = union . singleton

-- | /O(log n)/.
union :: Ord a => Heap a -> Heap a -> Heap a
union Leaf heap = heap
union heap Leaf = heap
union heap1@(Node _ _ x1 left1 right1) heap2@(Node _ _ x2 left2 right2)
    | x1 <= x2 = node x1 left1 (union right1 heap2)
    | otherwise = node x2 left2 (union right2 heap1)

unions :: (Foldable f, Ord a) => f (Heap a) -> Heap a
unions = foldl' union empty


map :: (Ord a, Ord b) => (a -> b) -> Heap a -> Heap b
map f = fromList . fmap f . toAscList  -- TEMP

-- | /O(n)/. It is assumed that f is increasing. The precondition is not checked.
mapMonotonic :: (a -> b) -> Heap a -> Heap b
mapMonotonic _ Leaf = Leaf
mapMonotonic f (Node s r x left right) = Node s r (f x) (mapMonotonic f left) (mapMonotonic f right)

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter _ Leaf = Leaf
filter f (Node _ _ x left right)
    | f x = node x (filter f left) (filter f right)
    | otherwise = union (filter f left) (filter f right)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition _ Leaf = (Leaf, Leaf)
partition f (Node _ _ x left right)
    | f x = (node x l1 r1, union l2 r2)
    | otherwise = (union l1 r1, node x l2 r2)
  where
    (l1, l2) = partition f left
    (r1, r2) = partition f right


member :: Ord a => a -> Heap a -> Bool
member _ Leaf = False
member x (Node _ _ y left right)
    | x == y = True
    | x < y = False
    | otherwise = member x left || member x right

-- | /O(1)/.
size :: Heap a -> Size
size Leaf = 0
size (Node s _ _ _ _) = s


-- | /O(1)/.
lookupMin :: Heap a -> Maybe a
lookupMin Leaf = Nothing
lookupMin (Node _ _ x _ _) = Just x

-- | /O(1)/.
findMin :: Heap a -> a
findMin heap = case lookupMin heap of
    Nothing -> errorEmpty "findMin"
    Just x -> x

-- | /O(log n)/.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Leaf = Leaf
deleteMin (Node _ _ _ left right) = union left right

-- | /O(log n)/.
deleteFindMin :: Ord a => Heap a -> (a, Heap a)
deleteFindMin heap = case minView heap of
    Nothing -> errorEmpty "deleteFindMin"
    Just res -> res

-- | /O(log n)/.
minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView Leaf = Nothing
minView (Node _ _ x left right) = Just (x, union left right)


toAscList :: Ord a => Heap a -> [a]
toAscList Leaf = []
toAscList (Node _ _ x left right) = x : toAscList (union left right)

toDescList :: Ord a => Heap a -> [a]
toDescList = go []
  where
    go acc Leaf = acc
    go acc (Node _ _ x left right) = go (x : acc) (union left right)
{-# INLINE toDescList #-}
