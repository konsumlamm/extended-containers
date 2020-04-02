module Data.Heap.Old
( Heap(..)
-- * Construction
, empty, singleton
-- ** From Lists
, fromList, fromAscList, fromDescList
-- * Insertion/Union
, insert
, union, unions
-- * Traversal/Filter
, map, mapMonotonic
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
, smallestN
, toAscList, toDescList
-- * Heapsort
, heapsort
) where

import Data.Foldable (foldl', toList)
import Data.Functor.Classes
import Data.Maybe (fromMaybe)
import Prelude hiding (filter, map)
import Text.Read (readListPrec, readPrec)

type Size = Int
type Rank = Int
-- | A Leftist Heap.
data Heap a = Leaf | Node {-# UNPACK #-} !Size {-# UNPACK #-} !Rank !a !(Heap a) !(Heap a)

node :: a -> Heap a -> Heap a -> Heap a
node x heap Leaf = Node (size heap + 1) 1 x heap Leaf
node x Leaf heap = Node (size heap + 1) 1 x heap Leaf
node x heap1@(Node s1 r1 _ _ _) heap2@(Node s2 r2 _ _ _)
    | r1 >= r2 = Node (s1 + s2 + 1) (r2 + 1) x heap1 heap2
    | otherwise = Node (s1 + s2 + 1) (r1 + 1) x heap2 heap1

errorEmpty :: String -> a
errorEmpty s = error $ "Heap." ++ s ++ ": empty heap"


instance Show1 Heap where
    liftShowsPrec sp sl p heap = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList heap)

instance Show a => Show (Heap a) where
    showsPrec = showsPrec1
    {-# INLINE showsPrec #-}

instance (Ord a, Read a) => Read (Heap a) where
    readPrec = readData $ readUnaryWith readListPrec "fromList" fromList

instance Ord a => Eq (Heap a) where
    heap1 == heap2 = size heap1 == size heap2 && toAscList heap1 == toAscList heap2

instance Ord a => Ord (Heap a) where
    compare heap1 heap2 = compare (toAscList heap1) (toAscList heap2)

instance Ord a => Semigroup (Heap a) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Ord a => Monoid (Heap a) where
    mempty = empty
    {-# INLINE mempty #-}

-- | Folds in an unspecified order.
instance Foldable Heap where
    foldr _ acc Leaf = acc
    foldr f acc (Node _ _ x left right) = f x (foldr f (foldr f acc right) left)

    null Leaf = True
    null Node{} = False

    length = size
    {-# INLINE length #-}

    minimum = findMin
    {-# INLINE minimum #-}


-- | /O(1)/. The empty heap.
--
-- > empty = fromList []
empty :: Heap a
empty = Leaf
{-# INLINE empty #-}

-- | /O(1)/. A heap with a single element.
--
-- > singleton x = fromList [x]
singleton :: a -> Heap a
singleton x = Node 1 1 x Leaf Leaf
{-# INLINE singleton #-}

-- | /O(n)/. Create a heap from a list.
fromList :: Ord a => [a] -> Heap a
fromList ls = fromList' (fmap singleton ls) []
  where
    fromList' [] [] = empty
    fromList' [x] [] = x
    fromList' (x1 : x2 : xs) ys = fromList' xs (union x1 x2 : ys)
    fromList' xs ys = fromList' (xs ++ reverse ys) []

-- | /O(n)/. Create a heap from a ascending list.
-- The precondition (list is ascending) is not checked.
fromAscList :: [a] -> Heap a
fromAscList = foldr (\x acc -> node x acc empty) empty

-- | /O(n)/. Create a heap from a descending list.
-- The precondition (list is descending) is not checked.
fromDescList :: [a] -> Heap a
fromDescList = foldl' (\acc x -> node x acc empty) empty

-- | /O(log n)/. Insert a new element into the heap.
insert :: Ord a => a -> Heap a -> Heap a
insert = union . singleton

-- | /O(log(max(n, m)))/. The union of two heaps.
union :: Ord a => Heap a -> Heap a -> Heap a
union Leaf heap = heap
union heap Leaf = heap
union heap1@(Node _ _ x1 left1 right1) heap2@(Node _ _ x2 left2 right2)
    | x1 <= x2 = node x1 left1 (right1 `union` heap2)
    | otherwise = node x2 left2 (right2 `union` heap1)

-- | The union of a foldable of heaps.
--
-- > unions = foldl union empty
unions :: (Foldable f, Ord a) => f (Heap a) -> Heap a
unions = foldl' union empty


-- | /O(n * log n)/. Map a function over the heap.
map :: (Ord a, Ord b) => (a -> b) -> Heap a -> Heap b
map f = fromList . fmap f . toAscList

-- | /O(n)/. Map an increasing function over the heap.
-- The precondition (if @x <= y@ then @f x <= f y@) is not checked.
mapMonotonic :: (a -> b) -> Heap a -> Heap b
mapMonotonic _ Leaf = Leaf
mapMonotonic f (Node s r x left right) = Node s r (f x) (mapMonotonic f left) (mapMonotonic f right)

-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter _ Leaf = Leaf
filter f (Node _ _ x left right)
    | f x = node x (filter f left) (filter f right)
    | otherwise = filter f left `union` filter f right

-- | /O(n)/. Partition the heap into two heaps, one with all elements that satisfy the predicate and one with all elements that don't satisfy the predicate.
partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition _ Leaf = (Leaf, Leaf)
partition f (Node _ _ x left right)
    | f x = (node x l1 r1, l2 `union` r2)
    | otherwise = (l1 `union` r1, node x l2 r2)
  where
    (l1, l2) = partition f left
    (r1, r2) = partition f right


-- | /O(n)/. Is the element in the heap?
member :: Ord a => a -> Heap a -> Bool
member _ Leaf = False
member x (Node _ _ y left right)
    | x == y = True
    | x < y = False
    | otherwise = member x left || member x right

-- | /O(1)/. The number of elements in the heap.
size :: Heap a -> Size
size Leaf = 0
size (Node s _ _ _ _) = s


-- | /O(1)/. The minimal element of the heap or 'Nothing' if the heap is empty.
lookupMin :: Heap a -> Maybe a
lookupMin Leaf = Nothing
lookupMin (Node _ _ x _ _) = Just x

-- | /O(1)/. The minimal element of the heap. Calls 'error' if the heap is empty.
findMin :: Heap a -> a
findMin heap = fromMaybe (errorEmpty "findMin") (lookupMin heap)

-- | /O(log n)/. Delete the minimal element. Returns the empty heap if the heap is empty.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Leaf = Leaf
deleteMin (Node _ _ _ left right) = left `union` right

-- | /O(log n)/. Delete and find the minimal element. Calls 'error' if the heap is empty.
--
-- > deleteFindMin heap = (findMin heap, deleteMin heap)
deleteFindMin :: Ord a => Heap a -> (a, Heap a)
deleteFindMin heap = fromMaybe (errorEmpty "deleteFindMin") (minView heap)

-- | /O(log n)/.  Retrieves the minimal element of the heap and the heap stripped of that element or 'Nothing' if the heap is empty.
minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView Leaf = Nothing
minView (Node _ _ x left right) = Just (x, left `union` right)


-- | /O(n * log n)/. Create an ascending list from the heap.
toAscList :: Ord a => Heap a -> [a]
toAscList Leaf = []
toAscList (Node _ _ x left right) = x : toAscList (left `union` right)

-- | /O(n * log n)/. Returns an ascending list of the n smallest elements from the heap and the remaining heap.
smallestN :: Ord a => Int -> Heap a -> ([a], Heap a)
smallestN _ Leaf = ([], Leaf)
smallestN n heap@(Node _ _ x left right)
    | n <= 0 = ([], heap)
    | otherwise = let (xs, heap') = smallestN (n - 1) (left `union` right) in (x : xs, heap')

-- | /O(n * log n)/. Create a descending list from the heap.
toDescList :: Ord a => Heap a -> [a]
toDescList = go []
  where
    go acc Leaf = acc
    go acc (Node _ _ x left right) = go (x : acc) (left `union` right)
{-# INLINE toDescList #-}

-- | /O(n * log n)/. Sort the elements of a list using a heap.
heapsort :: Ord a => [a] -> [a]
heapsort = toAscList . fromList
