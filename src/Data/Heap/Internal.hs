-- TODO: IsList instance?
-- TODO: takeWhile etc. (see psqueue, heap, heaps)?

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
    -- * Query
    , size
    -- * Min
    , lookupMin
    , findMin
    , deleteMin
    , deleteFindMin
    , minView
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
import Prelude hiding (filter, map, reverse)
import Text.Read (readPrec, readListPrec)

import Data.List.Strict

-- | A skew binomial heap.
data Heap a = Empty | Heap {-# UNPACK #-} !Int !a !(Forest a)

type Forest a = List (Tree a)

type Rank = Int

data Tree a = Node
    { _rank :: {-# UNPACK #-} !Rank
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


-- | /O(1)/. The empty heap.
empty :: Heap a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A heap with a single element.
singleton :: a -> Heap a
singleton x = Heap 1 x Nil
{-# INLINE singleton #-}

-- | /O(n * log n)/.
fromList :: Ord a => [a] -> Heap a
fromList = foldl' (flip insert) empty
{-# INLINE fromList #-}

-- | /O(log n)/.
insert :: Ord a => a -> Heap a -> Heap a
insert x Empty = singleton x
insert x (Heap s y f)
    | x <= y = Heap (s + 1) x (ins y f)
    | otherwise = Heap (s + 1) y (ins x f)

-- TODO: correct complexity?
-- | /O(log n)/.
union :: Ord a => Heap a -> Heap a -> Heap a
union heap Empty = heap
union Empty heap = heap
union (Heap s1 x1 f1) (Heap s2 x2 f2)
    | x1 <= x2 = Heap (s1 + s2) x1 (ins x2 (merge f1 f2))
    | otherwise = Heap (s1 + s2) x2 (ins x1 (merge f1 f2))

unions :: (Foldable f, Ord a) => f (Heap a) -> Heap a
unions = foldl' union empty
{-# INLINE unions #-}

map :: (Ord a, Ord b) => (a -> b) -> Heap a -> Heap b
map f = fromList . fmap f . toList
{-# INLINE map #-}

mapMonotonic :: (a -> b) -> Heap a -> Heap b
mapMonotonic _ Empty = Empty
mapMonotonic f (Heap s x forest) = Heap s (f x) (fmap (fmap f) forest)
{-# INLINE mapMonotonic #-}

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter f = foldl' (\acc x -> if f x then insert x acc else acc) empty
{-# INLINE filter #-}

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition f = foldl' (\(h1, h2) x -> if f x then (insert x h1, h2) else (h1, insert x h2)) (empty, empty)
{-# INLINE partition #-}

-- | /O(1)/.
size :: Heap a -> Int
size Empty = 0
size (Heap s _ _) = s
{-# INLINE size #-}

-- | /O(log n)/.
findMin :: Ord a => Heap a -> a
findMin Empty = error "findMin: empty heap"
findMin (Heap _ x _) = x
{-# INLINE findMin #-}

-- | /O(log n)/.
lookupMin :: Ord a => Heap a -> Maybe a
lookupMin Empty = Nothing
lookupMin (Heap _ x _) = Just $! x
{-# INLINE lookupMin #-}

-- | /O(log n)/.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Heap s _ f) = fromForest (s - 1) f
{-# INLINE deleteMin #-}

deleteFindMin :: Ord a => Heap a -> (a, Heap a)
deleteFindMin heap = fromMaybe (errorEmpty "deleteFindMin") (minView heap)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/.
minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView Empty = Nothing
minView (Heap s x f) = Just (x, fromForest (s - 1) f)
{-# INLINE minView #-}

-- | /O(n * log n)/. Create a descending list from the heap.
toAscList :: Ord a => Heap a -> [a]
toAscList h = case minView h of
    Nothing -> []
    Just (x, h') -> x : toAscList h'

-- | /O(n * log n)/. Create a descending list from the heap.
toDescList :: Ord a => Heap a -> [a]
toDescList = go []
  where
    go acc h = case minView h of
        Nothing -> acc
        Just (x, h') -> go (x : acc) h'
{-# INLINE toDescList #-}

-- | /O(n * log n)/.
heapsort :: Ord a => [a] -> [a]
heapsort = toAscList . fromList
{-# INLINE heapsort #-}
