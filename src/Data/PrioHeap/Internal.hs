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
, adjustMinWithKey
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
, values
, keys
, toList
, toAscList
, toDescList
) where

import Data.Foldable (foldl', foldr')
import Data.Functor.Classes
import Prelude hiding (filter, map)
import Text.Read (readPrec)

import qualified Data.Heap.Internal as Heap

type Size = Int
type Rank = Int
-- | A Leftist Heap with associated priorities.
data PrioHeap k a = Leaf | Node !Size !Rank !k a !(PrioHeap k a) !(PrioHeap k a)

node :: k -> a -> PrioHeap k a -> PrioHeap k a -> PrioHeap k a
node key x heap Leaf = Node (size heap + 1) 1 key x heap Leaf
node key x Leaf heap = Node (size heap + 1) 1 key x heap Leaf
node key x heap1@(Node s1 r1 _ _ _ _) heap2@(Node s2 r2 _ _ _ _)
    | r1 >= r2 = Node (s1 + s2 + 1) (r2 + 1) key x heap1 heap2
    | otherwise = Node (s1 + s2 + 1) (r1 + 1) key x heap2 heap1

errorEmpty :: String -> a
errorEmpty s = error $ "PrioHeap." ++ s ++ ": empty heap"


instance Show2 PrioHeap where
    liftShowsPrec2 spk slk spv slv p heap = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList heap)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (PrioHeap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show k, Show a) => Show (PrioHeap k a) where
    showsPrec = showsPrec2
    {-# INLINE showsPrec #-}

instance (Ord k, Read k) => Read1 (PrioHeap k) where
    liftReadPrec rp rl = readData $ readUnaryWith (liftReadPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadPrec rp rl
        rl' = liftReadListPrec rp rl

instance (Ord k, Read k, Read a) => Read (PrioHeap k a) where
    readPrec = readPrec1

instance Ord k => Eq1 (PrioHeap k) where
    liftEq f heap1 heap2 = size heap1 == size heap2 && liftEq (liftEq f) (toAscList heap1) (toAscList heap2)

instance (Ord k, Eq a) => Eq (PrioHeap k a) where
    (==) = eq1
    {-# INLINE (==) #-}

instance Ord k => Ord1 (PrioHeap k) where
    liftCompare f heap1 heap2 = liftCompare (liftCompare f) (toAscList heap1) (toAscList heap2)

instance (Ord k, Ord a) => Ord (PrioHeap k a) where
    compare = compare1
    {-# INLINE compare #-}

instance Ord k => Semigroup (PrioHeap k a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Ord k => Monoid (PrioHeap k a) where
    mempty = empty
    {-# INLINE mempty #-}

instance Functor (PrioHeap k) where
    fmap = map
    {-# INLINE fmap #-}

-- | Folds in an unspecified order.
instance Foldable (PrioHeap k) where
    foldr _ acc Leaf = acc
    foldr f acc (Node _ _ _ x left right) = f x (foldr f (foldr f acc right) left)

    null Leaf = True
    null (Node _ _ _ _ _ _) = False

    length = size
    {-# INLINE length #-}

    minimum = snd . findMin
    {-# INLINE minimum #-}

-- | Traverses in an unspecified order.
instance Traversable (PrioHeap k) where
    traverse f = traverseWithKey (const f)


-- | /O(1)/. The empty heap.
empty :: PrioHeap k a
empty = Leaf
{-# INLINE empty #-}

-- | /O(1)/. A heap with a single element.
singleton :: k -> a -> PrioHeap k a
singleton k x = Node 1 1 k x Leaf Leaf
{-# INLINE singleton #-}

-- | /O(n)/. Create a heap from a 'Data.Heap.Heap' of keys and a function which computes the value for each key.
fromHeap :: (k -> a) -> Heap.Heap k -> PrioHeap k a
fromHeap _ Heap.Leaf = Leaf
fromHeap f (Heap.Node s r x left right) = Node s r x (f x) (fromHeap f left) (fromHeap f right)


-- | /O(n)/. Create a heap from a list of key/value pairs.
fromList :: Ord k => [(k, a)] -> PrioHeap k a
fromList ls = fromList' (fmap (uncurry singleton) ls) []
  where
    fromList' [] [] = empty
    fromList' [x] [] = x
    fromList' (x1 : x2 : xs) ys = fromList' xs (union x1 x2 : ys)
    fromList' xs ys = fromList' (xs ++ reverse ys) []

-- | /O(n)/. Create a heap from an ascending list.
-- The precondition (list is ascending) is not checked.
fromAscList :: [(k, a)] -> PrioHeap k a
fromAscList = foldr (\(key, x) acc -> node key x acc empty) empty

-- | /O(n)/. Create a heap from a decreasing list.
-- The precondition (list is descending) is not checked.
fromDescList :: [(k, a)] -> PrioHeap k a
fromDescList = foldl' (\acc (key, x) -> node key x acc empty) empty


-- | /O(log n)/. Insert a new key and value into the heap.
insert :: Ord k => k -> a -> PrioHeap k a -> PrioHeap k a
insert key x = union (singleton key x)

-- | The union of two heaps.
union :: Ord k => PrioHeap k a -> PrioHeap k a -> PrioHeap k a
union Leaf heap = heap
union heap Leaf = heap
union heap1@(Node _ _ key1 x1 left1 right1) heap2@(Node _ _ key2 x2 left2 right2)
    | key1 <= key2 = node key1 x1 left1 (union right1 heap2)
    | otherwise = node key2 x2 left2 (union right2 heap1)

-- | The union of a foldable of heaps.
--
-- > unions = foldl union empty
unions :: (Foldable f, Ord k) => f (PrioHeap k a) -> PrioHeap k a
unions = foldl' union empty


-- | /O(n)/. Map a function over the heap.
map :: (a -> b) -> PrioHeap k a -> PrioHeap k b
map f = mapWithKey (const f)
{-# INLINE map #-}

-- | /O(n)/. Map a function that has access to the key associated with a value over the heap.
mapWithKey :: (k -> a -> b) -> PrioHeap k a -> PrioHeap k b
mapWithKey _ Leaf = Leaf
mapWithKey f (Node s r key x left right) =
    Node s r key (f key x) (mapWithKey f left) (mapWithKey f right)

-- | /O(n)/. Traverse the heap with a function that has access to the key associated with a value.
traverseWithKey :: Applicative t => (k -> a -> t b) -> PrioHeap k a -> t (PrioHeap k b)
traverseWithKey _ Leaf = pure Leaf
traverseWithKey f (Node s r key x left right) =
    Node s r key <$> f key x <*> traverseWithKey f left <*> traverseWithKey f right

-- | Filter all elements that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> PrioHeap k a -> PrioHeap k a
filter f = filterWithKey (const f)
{-# INLINE filter #-}

-- | Filter all elements that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> PrioHeap k a
filterWithKey _ Leaf = Leaf
filterWithKey f (Node _ _ key x left right)
    | f key x = node key x (filterWithKey f left) (filterWithKey f right)
    | otherwise = union (filterWithKey f left) (filterWithKey f right)

-- | Partition the heap into two heaps, one with all elements that satisfy the predicate
-- and one with all elements that don't satisfy the predicate.
partition :: Ord k => (a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partition f = partitionWithKey (const f)
{-# INLINE partition #-}

-- | Partition the heap into two heaps, one with all elements that satisfy the predicate
-- and one with all elements that don't satisfy the predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partitionWithKey _ Leaf = (Leaf, Leaf)
partitionWithKey f (Node _ _ key x left right)
    | f key x = (node key x l1 r1, union l2 r2)
    | otherwise = (union l1 r1, node key x l2 r2)
  where
    (l1, l2) = partitionWithKey f left
    (r1, r2) = partitionWithKey f right

-- | Map and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | Map and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybeWithKey _ Leaf = Leaf
mapMaybeWithKey f (Node _ _ key x left right) = case f key x of
    Nothing -> union (mapMaybeWithKey f left) (mapMaybeWithKey f right)
    Just x' -> node key x' (mapMaybeWithKey f left) (mapMaybeWithKey f right)

-- | Map and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEither f = mapEitherWithKey (const f)
{-# INLINE mapEither #-}

-- | Map and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEitherWithKey _ Leaf = (Leaf, Leaf)
mapEitherWithKey f (Node _ _ key x left right) = case f key x of
    Left y -> (node key y l1 r1, union l2 r2)
    Right z -> (union l1 r1, node key z l2 r2)
  where
    (l1, l2) = mapEitherWithKey f left
    (r1, r2) = mapEitherWithKey f right

-- | /O(n)/. Fold the keys and values in the heap using the given monoid.
foldMapWithKey :: Monoid m => (k -> a -> m) -> PrioHeap k a -> m
foldMapWithKey _ Leaf = mempty
foldMapWithKey f (Node _ _ key x left right) =
    f key x <> foldMapWithKey f left <> foldMapWithKey f right

-- | /O(n)/. Fold the keys and values in the heap using the given left-associative function.
foldlWithKey :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey _ acc Leaf = acc
foldlWithKey f acc (Node _ _ key x left right) = foldlWithKey f (foldlWithKey f (f acc key x) left) right

-- | /O(n)/. Fold the keys and values in the heap using the given right-associative function.
foldrWithKey :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey _ acc Leaf = acc
foldrWithKey f acc (Node _ _ key x left right) = f key x (foldrWithKey f (foldrWithKey f acc right) left)

-- | /O(n)/. A strict version of 'foldlWithKey'.
-- Each application of the function is evaluated before using the result in the next application. REDO!
foldlWithKey' :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey' f acc = foldl' (\a (key, x) -> f a key x) acc . toList

-- | /O(n)/. A strict version of 'foldrWithKey'.
-- Each application of the function is evaluated before using the result in the next application. REDO!
foldrWithKey' :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey' f acc = foldr' (\(key, x) a -> f key x a) acc . toList


-- | /O(n)/. Is the key in the heap?
member :: Ord k => k -> PrioHeap k a -> Bool
member _ Leaf = False
member kx (Node _ _ ky _ left right)
    | kx == ky = True
    | kx < ky = False
    | otherwise = member kx left || member kx right

-- | /O(1)/. The number of elements in the heap.
size :: PrioHeap k a -> Size
size Leaf = 0
size (Node s _ _ _ _ _) = s


-- | /O(1)/. Adjust the value at the minimal key.
adjustMin :: (a -> a) -> PrioHeap k a -> PrioHeap k a
adjustMin f = adjustMinWithKey (const f)
{-# INLINE adjustMin #-}

-- | /O(1)/. Adjust the value at the minimal key.
adjustMinWithKey :: (k -> a -> a) -> PrioHeap k a -> PrioHeap k a
adjustMinWithKey _ Leaf = Leaf
adjustMinWithKey f (Node s r key x left right) =
    Node s r key (f key x) left right

-- | /O(1)/. The minimal element of the heap or 'Nothing' if the heap is empty. 
lookupMin :: PrioHeap k a -> Maybe (k, a)
lookupMin Leaf = Nothing
lookupMin (Node _ _ key x _ _) = Just (key, x)

-- | /O(1)/. The minimal element of the heap. Calls 'error' if the heap is empty.
findMin :: PrioHeap k a -> (k, a)
findMin heap = case lookupMin heap of
    Nothing -> errorEmpty "findMin"
    Just res -> res

-- | /O(log n)/. Delete the minimal element. Returns the empty heap if the heap is empty.
deleteMin :: Ord k => PrioHeap k a -> PrioHeap k a
deleteMin Leaf = empty
deleteMin (Node _ _ _ _ left right) = union left right

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin heap = (findMin heap, deleteMin heap)
deleteFindMin :: Ord k => PrioHeap k a -> ((k, a), PrioHeap k a)
deleteFindMin heap = case minView heap of
    Nothing -> errorEmpty "deleteFindMin"
    Just res -> res

-- | /O(log n)/. Update the value at the minimal key.
updateMin :: Ord k => (a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMin f = updateMinWithKey (const f)
{-# INLINE updateMin #-}

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMinWithKey _ Leaf = Leaf
updateMinWithKey f (Node s r key x left right) = case f key x of
    Nothing -> union left right
    Just x' -> Node s r key x' left right

-- | /O(log n)/. Retrieves the minimal key/value pair of the heap and the heap stripped of that element or 'Nothing' if the heap is empty.
minView :: Ord k => PrioHeap k a -> Maybe ((k, a), PrioHeap k a)
minView Leaf = Nothing
minView (Node _ _ key x left right) = Just ((key, x), union left right)


-- take, drop, splitAt?


-- | Create a 'Data.Heap.Heap' of all keys of the heap
keysHeap :: PrioHeap k a -> Heap.Heap k
keysHeap Leaf = Heap.Leaf
keysHeap (Node s r key _ left right) = Heap.Node s r key (keysHeap left) (keysHeap right)


-- | /O(n)/. The values of the heap in an unspecified order.
values :: PrioHeap k a -> [a]
values = foldr (:) []

-- | /O(n)/. The keys of the heap in an unspecified order.
keys :: PrioHeap k a -> [k]
keys = foldrWithKey (\key _ acc -> key : acc) []

-- | /O(n)/. Create a list of key/value pairs from the heap.
toList :: PrioHeap k a -> [(k, a)]
toList = foldrWithKey (\key x acc -> (key, x) : acc) []

-- | /O(n * log n)/. Create an ascending list of key/value pairs from the heap.
toAscList :: Ord k => PrioHeap k a -> [(k, a)]
toAscList Leaf = []
toAscList (Node _ _ key x left right) = (key, x) : toAscList (union left right)

-- | /O(n * log n)/. Create a descending list of key/value pairs from the heap.
toDescList :: Ord k => PrioHeap k a -> [(k, a)]
toDescList = go []
  where
    go acc Leaf = acc
    go acc (Node _ _ key x left right) = go ((key, x) : acc) (union left right)
{-# INLINE toDescList #-}
