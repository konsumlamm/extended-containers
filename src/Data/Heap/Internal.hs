{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeFamilies #-}
#endif

module Data.Heap.Internal
    ( Heap(..)
    , Tree(..)
    -- * Construction
    , empty, singleton
    -- ** From Lists
    , fromList
    -- * Insertion/Union
    , insert
    , union, unions
    -- * Traversal/Filter
    , map, mapMonotonic
    , filter
    , partition
    -- * Ordered Folds
    , foldMapOrd
    , foldlOrd, foldrOrd
    , foldlOrd', foldrOrd'
    -- * Query
    , size
    , member, notMember
    -- * Min
    , lookupMin
    , findMin
    , deleteMin
    , deleteFindMin
    , minView
    -- * Subranges
    , take
    , drop
    , splitAt
    , takeWhile
    , dropWhile
    , span
    , break
    , nub
    -- * Conversion
    -- ** To Lists
    , toAscList, toDescList
    -- * Heapsort
    , heapsort
    ) where

import Control.Exception (assert)
import Data.Foldable (foldl', toList)
import Data.Functor.Classes
import Data.Maybe (fromMaybe)
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
#endif
import Prelude hiding (break, drop, dropWhile, filter, map, reverse, span, splitAt, take, takeWhile)
import Text.Read (readPrec, readListPrec)

import Util.Internal.StrictList

-- | A skew binomial heap.
data Heap a
    = Empty
    | Heap
        {-# UNPACK #-} !Int  -- size
        !a  -- root
        !(Forest a)  -- forest

type Forest a = List (Tree a)

data Tree a = Node
    { _rank :: {-# UNPACK #-} !Int
    , _root :: !a
    , _elements :: !(List a)
    , _children :: !(Forest a)
    }

errorEmpty :: String -> a
errorEmpty s = error $ "Heap." ++ s ++ ": empty heap"

instance Functor Tree where
    fmap f (Node r x xs c) = Node r (f x) (fmap f xs) (fmap (fmap f) c)

instance Foldable Tree where
    foldr f acc (Node _ x xs c) = f x (foldr f (foldr (flip (foldr f)) acc c) xs)

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r1 x1 xs1 c1) t2@(Node r2 x2 xs2 c2) = assert (r1 == r2) $
    if x1 <= x2
        then Node (r1 + 1) x1 xs1 (t2 `Cons` c1)
        else Node (r2 + 1) x2 xs2 (t1 `Cons` c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = let Node r y ys c = link t1 t2
    in if x <= y
        then Node r x (y `Cons` ys) c
        else Node r y (x `Cons` ys) c

insTree :: Ord a => Tree a -> Forest a -> Forest a
insTree t Nil = t `Cons` Nil
insTree t1 f@(t2 `Cons` ts)
    | _rank t1 < _rank t2 = t1 `Cons` f
    | otherwise = insTree (link t1 t2) ts

mergeTrees :: Ord a => Forest a -> Forest a -> Forest a
mergeTrees f Nil = f
mergeTrees Nil f = f
mergeTrees f1@(t1 `Cons` ts1) f2@(t2 `Cons` ts2) = case _rank t1 `compare` _rank t2 of
    LT -> t1 `Cons` mergeTrees ts1 f2
    GT -> t2 `Cons` mergeTrees f1 ts2
    EQ -> insTree (link t1 t2) (mergeTrees ts1 ts2)

merge :: Ord a => Forest a -> Forest a -> Forest a
merge f1 f2 = mergeTrees (normalize f1) (normalize f2)
{-# INLINE merge #-}

normalize :: Ord a => Forest a -> Forest a
normalize Nil = Nil
normalize (t `Cons` ts) = insTree t ts
{-# INLiNE normalize #-}

ins :: Ord a => a -> Forest a -> Forest a
ins x (t1 `Cons` t2 `Cons` ts)
    | _rank t1 == _rank t2 = x `seq` skewLink x t1 t2 `Cons` ts
ins x ts = x `seq` Node 0 x Nil Nil `Cons` ts

fromForest :: Ord a => Int -> Forest a -> Heap a
fromForest _ Nil = Empty
fromForest s f@(_ `Cons` _) =
    let (Node _ x xs ts1, ts2) = removeMinTree f
    in Heap s x (foldl' (flip ins) (merge (reverse ts1) ts2) xs)

removeMinTree :: Ord a => Forest a -> (Tree a, Forest a)
removeMinTree Nil = error "removeMinTree: empty heap"
removeMinTree (t `Cons` Nil) = (t, Nil)
removeMinTree (t `Cons` ts) =
    let (t', ts') = removeMinTree ts
    in if _root t <= _root t'
        then (t, ts)
        else (t', t `Cons` ts')

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

instance Foldable Heap where
    foldr _ acc Empty = acc
    foldr f acc (Heap _ x forest) = f x (foldr (flip (foldr f)) acc forest)

    null Empty = True
    null Heap{} = False
    {-# INLINE null #-}

    length = size
    {-# INLINE length #-}

    minimum = findMin
    {-# INLINE minimum #-}

#ifdef __GLASGOW_HASKELL__
instance Ord a => IsList (Heap a) where
    type Item (Heap a) = a

    fromList = fromList
    {-# INLINE fromList #-}

    toList = toList
    {-# INLINE toList #-}
#endif


-- | /O(1)/. The empty heap.
--
-- > empty = fromList []
empty :: Heap a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A heap with a single element.
--
-- > singleton x = fromList [x]
singleton :: a -> Heap a
singleton x = Heap 1 x Nil
{-# INLINE singleton #-}

-- | /O(n)/. Create a heap from a list.
fromList :: Ord a => [a] -> Heap a
fromList = foldl' (flip insert) empty
{-# INLINE fromList #-}

-- | /O(1)/. Insert a new value into the heap.
insert :: Ord a => a -> Heap a -> Heap a
insert x Empty = singleton x
insert x (Heap s y f)
    | x <= y = Heap (s + 1) x (ins y f)
    | otherwise = Heap (s + 1) y (ins x f)

-- | /O(log n)/. The union of two heaps.
union :: Ord a => Heap a -> Heap a -> Heap a
union heap Empty = heap
union Empty heap = heap
union (Heap s1 x1 f1) (Heap s2 x2 f2)
    | x1 <= x2 = Heap (s1 + s2) x1 (ins x2 (merge f1 f2))
    | otherwise = Heap (s1 + s2) x2 (ins x1 (merge f1 f2))

-- | The union of a foldable of heaps.
--
-- > unions = foldl union empty
unions :: (Foldable f, Ord a) => f (Heap a) -> Heap a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(n)/. Map a function over the heap.
map :: Ord b => (a -> b) -> Heap a -> Heap b
map f = fromList . fmap f . toList
{-# INLINE map #-}

-- | /O(n)/, Map an increasing function over the heap. The precondition is not checked.
mapMonotonic :: (a -> b) -> Heap a -> Heap b
mapMonotonic _ Empty = Empty
mapMonotonic f (Heap s x forest) = Heap s (f x) (fmap (fmap f) forest)
{-# INLINE mapMonotonic #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter f = foldl' (\acc x -> if f x then insert x acc else acc) empty
{-# INLINE filter #-}

-- | /O(n)/. Partition the heap into two heaps, one with all elements that satisfy the predicate
-- and one with all elements that don't satisfy the predicate.
partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition f = foldl' (\(h1, h2) x -> if f x then (insert x h1, h2) else (h1, insert x h2)) (empty, empty)
{-# INLINE partition #-}

-- | /O(n * log n)/. Fold the values in the heap in order, using the given monoid.
foldMapOrd :: (Ord a, Monoid m) => (a -> m) -> Heap a -> m
foldMapOrd f = foldrOrd ((<>) . f) mempty

-- | /O(n * log n)/. Fold the values in the heap in order, using the given right-associative function.
foldrOrd :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldrOrd f acc = go
  where
    go h = case minView h of
        Nothing -> acc
        Just (x, h') -> f x (go h')

-- | /O(n * log n)/. Fold the values in the heap in order, using the given left-associative function.
foldlOrd :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldlOrd f = go
  where
    go acc h = case minView h of
        Nothing -> acc
        Just (x, h') -> go (f acc x) h'

-- | /O(n * log n)/. A strict version of 'foldrOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldrOrd' :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldrOrd' f acc h = foldlOrd f' id h acc
  where
    f' k x z = k $! f x z
{-# INLINE foldrOrd' #-}

-- | /O(n)/. A strict version of 'foldlOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldlOrd' :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldlOrd' f acc h = foldrOrd f' id h acc
  where
    f' x k z = k $! f z x
{-# INLINE foldlOrd' #-}

-- | /O(1)/. The number of elements in the heap.
size :: Heap a -> Int
size Empty = 0
size (Heap s _ _) = s
{-# INLINE size #-}

-- | /O(n)/. Is the value a member of the heap?
member :: Ord a => a -> Heap a -> Bool
member _ Empty = False
member x (Heap _ y forest) = x <= y && any (x `elemTree`) forest
  where
    x `elemTree` (Node _ y ys c) = x <= y && (x `elem` ys || any (x `elemTree`) c)

-- | /O(n)/. Is the value not a member of the heap?
notMember :: Ord a => a -> Heap a -> Bool
notMember x = not . member x

-- | /O(log n)/. The minimal element in the heap. Calls 'error' if the heap is empty.
findMin :: Heap a -> a
findMin Empty = error "findMin: empty heap"
findMin (Heap _ x _) = x
{-# INLINE findMin #-}

-- | /O(log n)/. The minimal element in the heap or 'Nothing' if the heap is empty.
lookupMin :: Heap a -> Maybe a
lookupMin Empty = Nothing
lookupMin (Heap _ x _) = Just $! x
{-# INLINE lookupMin #-}

-- | /O(log n)/. Delete the minimal element. Returns the empty heap if the heap is empty.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Heap s _ f) = fromForest (s - 1) f
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete and find the minimal element. Calls 'error' if the heap is empty.
--
-- > deleteFindMin heap = (findMin heap, deleteMin heap)
deleteFindMin :: Ord a => Heap a -> (a, Heap a)
deleteFindMin heap = fromMaybe (errorEmpty "deleteFindMin") (minView heap)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Retrieves the minimal element of the heap and the heap stripped of that element or 'Nothing' if the heap is empty.
minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView Empty = Nothing
minView (Heap s x f) = Just (x, fromForest (s - 1) f)
{-# INLINE minView #-}

-- | /O(n * log n)/. @take n heap@ takes the @n@ smallest elements of @heap@, in ascending order.
--
-- > take n heap = take n (toAscList heap)
take :: Ord a => Int -> Heap a -> [a]
take n h
    | n <= 0 = []
    | otherwise = case minView h of
        Nothing -> []
        Just (x, h') -> x : take (n - 1) h'

-- | /O(n * log n)/. @drop n heap@ drops the @n@ smallest elements from @heap@.
drop :: Ord a => Int -> Heap a -> Heap a
drop n h
    | n <= 0 = h
    | otherwise = drop (n - 1) (deleteMin h)

-- | /O(n * log n)/. @splitAt n heap@ takes and drops the @n@ smallest elements from @heap@.
--
-- > splitAt n heap = (take n heap, drop n heap)
splitAt :: Ord a => Int -> Heap a -> ([a], Heap a)
splitAt n h
    | n <= 0 = ([], h)
    | otherwise = case minView h of
        Nothing -> ([], h)
        Just (x, h') -> let (xs, h'') = splitAt (n - 1) h' in (x : xs, h'')

-- | /O(n * log n)/. @takeWhile p heap@ takes the elements from @heap@ in ascending order, while @p@ holds.
takeWhile :: Ord a => (a -> Bool) -> Heap a -> [a]
takeWhile p = go
  where
    go h = case minView h of
        Nothing -> []
        Just (x, h') -> if p x then x : go h' else []
{-# INLINE takeWhile #-}

-- | /O(n * log n)/. @dropWhile p heap@ drops the elements from @heap@ in ascending order, while @p@ holds.
dropWhile :: Ord a => (a -> Bool) -> Heap a -> Heap a
dropWhile p = go
  where
    go h = case minView h of
        Nothing -> h
        Just (x, h') -> if p x then go h' else h
{-# INLINE dropWhile #-}

-- | /O(n * log n)/. @span p heap@ takes and drops the elements from @heap@, while @p@ holds
--
-- > span p heap = (takeWhile p heap, dropWhile p heap)
span :: Ord a => (a -> Bool) -> Heap a -> ([a], Heap a)
span p = go
  where
    go h = case minView h of
        Nothing -> ([], h)
        Just (x, h') -> if p x
            then let (xs, h'') = go h' in (x : xs, h'')
            else ([], h)
{-# INLINE span #-}

-- | /O(n * log n)/. @span@, but with inverted predicate.
--
-- > break p = span (not . p)
break :: Ord a => (a -> Bool) -> Heap a -> ([a], Heap a)
break p = span (not . p)
{-# INLINE break #-}

-- | /O(n * log n)/. Remove duplicate elements from the heap.
nub :: Ord a => Heap a -> Heap a
nub h = case minView h of
    Nothing -> Empty
    Just (x, h') -> insert x (nub (dropWhile (== x) h'))

-- | /O(n * log n)/. Create a descending list from the heap.
toAscList :: Ord a => Heap a -> [a]
toAscList = foldrOrd (:) []
{-# INLINE toAscList #-}

-- | /O(n * log n)/. Create a descending list from the heap.
toDescList :: Ord a => Heap a -> [a]
toDescList = foldlOrd (flip (:)) []
{-# INLINE toDescList #-}

-- | /O(n * log n)/. Sort a list using a heap. The sort is unstable.
heapsort :: Ord a => [a] -> [a]
heapsort = toAscList . fromList
{-# INLINE heapsort #-}
