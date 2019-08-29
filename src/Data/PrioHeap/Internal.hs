module Data.PrioHeap.Internal
( PrioHeap(..)
-- * Construction
, empty
, singleton
, fromHeap
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
, mapWithKey
, traverseWithKey
, filter
, filterWithKey
, partition
, partitionWithKey
, mapMaybe
, mapMaybeWithKey
, mapEither
, mapEitherWithKey
-- * Folds with key
, foldMapWithKey
, foldlWithKey
, foldrWithKey
, foldlWithKey'
, foldrWithKey'
-- * Query
, member
, size
-- * Min
, adjustMin
, lookupMin
, findMin
, deleteMin
, deleteFindMin
, updateMin
, updateMinWithKey
, minView
-- * Conversion
, keysHeap
-- ** To Lists
, elems
, keys
, toList
, toAscList
, toDescList
) where

import Prelude hiding (filter, map)
import Data.Foldable (foldl', foldr')

import qualified Data.Heap.Internal as Heap

-- A Leftist Heap with associated priorities.
type Size = Int
type Rank = Int
data PrioHeap k a = Leaf | Node !Size !Rank !k a !(PrioHeap k a) !(PrioHeap k a)

node :: k -> a -> PrioHeap k a -> PrioHeap k a -> PrioHeap k a
node key x heap Leaf = Node (size heap + 1) 1 key x heap Leaf
node key x Leaf heap = Node (size heap + 1) 1 key x heap Leaf
node key x heap1@(Node s1 r1 _ _ _ _) heap2@(Node s2 r2 _ _ _ _)
    | r1 >= r2 = Node (s1 + s2) (r2 + 1) key x heap1 heap2
    | otherwise = Node (s1 + s2) (r1 + 1) key x heap2 heap1

errorEmpty :: String -> a
errorEmpty s = error $ "PrioHeap." ++ s ++ ": empty heap"


instance Ord k => Semigroup (PrioHeap k a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Ord k => Monoid (PrioHeap k a) where
    mempty = empty
    {-# INLINE mempty #-}

instance Functor (PrioHeap k) where
    fmap = map
    {-# INLINE fmap #-}

instance Foldable (PrioHeap k) where
    foldMap _ Leaf = mempty
    foldMap f (Node _ _ _ x left right) =
        f x <> foldMap f left <> foldMap f right

    null Leaf = True
    null (Node _ _ _ _ _ _) = False

    length = size
    {-# INLINE length #-}

    minimum = snd . findMin
    {-# INLINE minimum #-}

instance Traversable (PrioHeap k) where
    traverse f = traverseWithKey (const f)


-- | /O(1)/.
empty :: PrioHeap k a
empty = Leaf
{-# INLINE empty #-}

-- | /O(1)/.
singleton :: k -> a -> PrioHeap k a
singleton k x = Node 1 1 k x Leaf Leaf
{-# INLINE singleton #-}

-- | /O(n)/.
fromHeap :: (k -> a) -> Heap.Heap k -> PrioHeap k a
fromHeap _ Heap.Leaf = Leaf
fromHeap f (Heap.Node s r x left right) = Node s r x (f x) (fromHeap f left) (fromHeap f right)


fromList :: Ord k => [(k, a)] -> PrioHeap k a
fromList ls = fromList' (fmap (uncurry singleton) ls) []
  where
    fromList' [] [] = empty
    fromList' [x] [] = x
    fromList' (x1 : x2 : xs) ys = fromList' xs (union x1 x2 : ys)
    fromList' xs ys = fromList' (xs ++ reverse ys) []

-- | /O(n)/. The precondition is not checked.
fromAscList :: Ord k => [(k, a)] -> PrioHeap k a
fromAscList = foldr (\(key, x) acc -> node key x acc empty) empty

-- | The precondition is not checked.
fromDescList :: Ord k => [(k, a)] -> PrioHeap k a
fromDescList = foldl' (\acc (key, x) -> node key x acc empty) empty


-- | /O(log n)/.
insert :: Ord k => k -> a -> PrioHeap k a -> PrioHeap k a
insert key x = union (singleton key x)

-- | /O(log n)/.
union :: Ord k => PrioHeap k a -> PrioHeap k a -> PrioHeap k a
union Leaf heap = heap
union heap Leaf = heap
union heap1@(Node _ _ key1 x1 left1 right1) heap2@(Node _ _ key2 x2 left2 right2)
    | key1 <= key2 = node key1 x1 left1 (union right1 heap2)
    | otherwise = node key2 x2 left2 (union right2 heap1)

unions :: (Foldable f, Ord k) => f (PrioHeap k a) -> PrioHeap k a
unions = foldl' union empty


-- | /O(n)/.
map :: (a -> b) -> PrioHeap k a -> PrioHeap k b
map f = mapWithKey (const f)

-- | /O(n)/.
mapWithKey :: (k -> a -> b) -> PrioHeap k a -> PrioHeap k b
mapWithKey _ Leaf = Leaf
mapWithKey f (Node s r key x left right) =
    Node s r key (f key x) (mapWithKey f left) (mapWithKey f right)

-- | /O(n)/.
traverseWithKey :: Applicative t => (k -> a -> t b) -> PrioHeap k a -> t (PrioHeap k b)
traverseWithKey _ Leaf = pure Leaf
traverseWithKey f (Node s r key x left right) =
    Node s r key <$> f key x <*> traverseWithKey f left <*> traverseWithKey f right

filter :: Ord k => (a -> Bool) -> PrioHeap k a -> PrioHeap k a
filter f = filterWithKey (const f)

filterWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> PrioHeap k a
filterWithKey _ Leaf = Leaf
filterWithKey f (Node _ _ key x left right)
    | f key x = node key x (filterWithKey f left) (filterWithKey f right)
    | otherwise = union (filterWithKey f left) (filterWithKey f right)

partition :: Ord k => (a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partition f = partitionWithKey (const f)

partitionWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partitionWithKey _ Leaf = (Leaf, Leaf)
partitionWithKey f (Node _ _ key x left right)
    | f key x = (node key x l1 r1, union l2 r2)
    | otherwise = (union l1 r1, node key x l2 r2)
  where
    (l1, l2) = partitionWithKey f left
    (r1, r2) = partitionWithKey f right

mapMaybe :: Ord k => (a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybeWithKey _ Leaf = Leaf
mapMaybeWithKey f (Node _ _ key x left right) = case f key x of
    Nothing -> union (mapMaybeWithKey f left) (mapMaybeWithKey f right)
    Just x' -> node key x' (mapMaybeWithKey f left) (mapMaybeWithKey f right)

mapEither :: Ord k => (a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEither f = mapEitherWithKey (const f)

mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEitherWithKey _ Leaf = (Leaf, Leaf)
mapEitherWithKey f (Node _ _ key x left right) = case f key x of
    Left y -> (node key y l1 r1, union l2 r2)
    Right z -> (union l1 r1, node key z l2 r2)
  where
    (l1, l2) = mapEitherWithKey f left
    (r1, r2) = mapEitherWithKey f right

foldMapWithKey :: Monoid m => (k -> a -> m) -> PrioHeap k a -> m
foldMapWithKey _ Leaf = mempty
foldMapWithKey f (Node _ _ key x left right) =
    f key x <> foldMapWithKey f left <> foldMapWithKey f right

foldlWithKey :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey f acc = foldl (\a (key, x) -> f a key x) acc . toList

foldrWithKey :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey f acc = foldr (\(key, x) a -> f key x a) acc . toList

foldlWithKey' :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey' f acc = foldl' (\a (key, x) -> f a key x) acc . toList

foldrWithKey' :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey' f acc = foldr' (\(key, x) a -> f key x a) acc . toList


member :: Ord k => k -> PrioHeap k a -> Bool
member _ Leaf = False
member kx (Node _ _ ky _ left right)
    | kx == ky = True
    | kx < ky = False
    | otherwise = member kx left || member kx right

-- | /O(1)/.
size :: PrioHeap k a -> Size
size Leaf = 0
size (Node s _ _ _ _ _) = s


-- | /O(1)/.
adjustMin :: (a -> a) -> PrioHeap k a -> PrioHeap k a
adjustMin _ Leaf = Leaf
adjustMin f (Node s r key x left right) =
    Node s r key (f x) left right

-- | /O(1)/.
lookupMin :: PrioHeap k a -> Maybe (k, a)
lookupMin Leaf = Nothing
lookupMin (Node _ _ key x _ _) = Just (key, x)

-- | /O(1)/.
findMin :: PrioHeap k a -> (k, a)
findMin heap = case lookupMin heap of
    Nothing -> errorEmpty "findMin"
    Just res -> res

-- | /O(log n)/.
deleteMin :: Ord k => PrioHeap k a -> PrioHeap k a
deleteMin Leaf = empty
deleteMin (Node _ _ _ _ left right) = union left right

-- | /O(log n)/.
deleteFindMin :: Ord k => PrioHeap k a -> ((k, a), PrioHeap k a)
deleteFindMin heap = case minView heap of
    Nothing -> errorEmpty "deleteFindMin"
    Just res -> res

-- | /O(log n)/.
updateMin :: Ord k => (a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMin f = updateMinWithKey (const f)

-- | /O(log n)/.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMinWithKey _ Leaf = Leaf
updateMinWithKey f (Node s r key x left right) = case f key x of
    Nothing -> union left right
    Just x' -> Node s r key x' left right

-- | /O(log n)/.
minView :: Ord k => PrioHeap k a -> Maybe ((k, a), PrioHeap k a)
minView Leaf = Nothing
minView (Node _ _ key x left right) = Just ((key, x), union left right)


-- take, drop, splitAt?


keysHeap :: PrioHeap k a -> Heap.Heap k
keysHeap Leaf = Heap.Leaf
keysHeap (Node s r key _ left right) = Heap.Node s r key (keysHeap left) (keysHeap right)


elems :: PrioHeap k a -> [a]
elems = foldr (:) []

keys :: PrioHeap k a -> [k]
keys = foldrWithKey (\key _ acc -> key : acc) []

toList :: PrioHeap k a -> [(k, a)]
toList = foldrWithKey (\key x acc -> (key, x) : acc) []

toAscList :: Ord k => PrioHeap k a -> [(k, a)]
toAscList Leaf = []
toAscList (Node _ _ key x left right) = (key, x) : toAscList (union left right)

toDescList :: Ord k => PrioHeap k a -> [(k, a)]
toDescList = go []
  where
    go acc Leaf = acc
    go acc (Node _ _ key x left right) = go ((key, x) : acc) (union left right)
