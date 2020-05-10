{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeFamilies #-}
#endif

{- |
= Finite priority heaps

The @'PrioHeap' k a@ type represents a finite heap (or priority queue) from keys/priorities of type @k@ to values of type @a@.
A 'PrioHeap' is strict in its spine. Unlike with maps, duplicate keys/priorities are allowed.

== Performance

The worst case running time complexities are given, with /n/ referring the the number of elements in the heap.

== Warning

The length of a 'PrioHeap' must not exceed @'maxBound' :: 'Int'@.
Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the heap is undefined.

== Implementation

The implementation uses skew binomial heaps, as described in

* Chris Okasaki, \"Purely Functional Data Structures\", 1998
-}

module Data.PrioHeap
    ( PrioHeap
    -- * Construction
    , empty, singleton
    , fromHeap
    -- ** From Lists
    , fromList
    -- * Insertion/Union
    , insert
    , union, unions
    -- * Traversal/Filter
    , map, mapWithKey
    , traverseWithKey
    , filter, filterWithKey
    , partition, partitionWithKey
    , mapMaybe, mapMaybeWithKey
    , mapEither, mapEitherWithKey
    -- * Folds
    , foldMapWithKey
    , foldlWithKey, foldrWithKey
    , foldlWithKey', foldrWithKey'
    , foldMapOrd
    , foldlOrd, foldrOrd
    , foldlOrd', foldrOrd'
    , foldMapWithKeyOrd
    , foldlWithKeyOrd, foldrWithKeyOrd
    , foldlWithKeyOrd', foldrWithKeyOrd'
    -- * Query
    , size
    , member, notMember
    -- * Min
    , adjustMin, adjustMinWithKey
    , lookupMin
    , findMin
    , deleteMin
    , deleteFindMin
    , updateMin, updateMinWithKey
    , minView
    -- * Subranges
    , take
    , drop
    , splitAt
    , takeWhile, takeWhileWithKey
    , dropWhile, dropWhileWithKey
    , span, spanWithKey
    , break, breakWithKey
    , nub
    -- * Conversion
    , keysHeap
    -- ** To Lists
    , toList, toAscList, toDescList
    ) where

import Control.Exception (assert)
import Data.Foldable (foldl', foldr')
import Data.Functor.Classes
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
#endif
import Prelude hiding (break, drop, dropWhile, filter, map, reverse, span, splitAt, take, takeWhile, uncurry)
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec)

import Control.DeepSeq

import qualified Data.Heap.Internal as Heap
import Util.Internal.StrictList

-- | A skew binomial heap with associated priorities.
data PrioHeap k a
    = Empty
    | Heap
        {-# UNPACK #-} !Int  -- size
        !k  -- root key
        a  -- root value
        !(Forest k a)  -- forest

type Forest k a = List (Tree k a)

data Pair k a = Pair !k a

data Tree k a = Node
    { _rank :: {-# UNPACK #-} !Int
    , _root :: !k
    , _value :: a
    , _elements :: !(List (Pair k a))
    , _children :: !(Forest k a)
    }

instance (NFData k, NFData a) => NFData (Pair k a) where
    rnf (Pair k x) = rnf k `seq` rnf x

instance (NFData k, NFData a) => NFData (Tree k a) where
    rnf (Node _ k x xs c) = rnf k `seq` rnf x `seq` rnf xs `seq` rnf c

errorEmpty :: String -> a
errorEmpty s = error $ "PrioHeap." ++ s ++ ": empty heap"

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (Pair x y) = f x y
{-# INLINE uncurry #-}

link :: Ord k => Tree k a -> Tree k a -> Tree k a
link t1@(Node r1 key1 x1 xs1 c1) t2@(Node r2 key2 x2 xs2 c2) = assert (r1 == r2) $
    if key1 <= key2
        then Node (r1 + 1) key1 x1 xs1 (t2 `Cons` c1)
        else Node (r2 + 1) key2 x2 xs2 (t1 `Cons` c2)

skewLink :: Ord k => k -> a -> Tree k a -> Tree k a -> Tree k a
skewLink kx x t1 t2 = let Node r ky y ys c = link t1 t2
    in if kx <= ky
        then Node r kx x (Pair ky y `Cons` ys) c
        else Node r ky y (Pair kx x `Cons` ys) c

insTree :: Ord k => Tree k a -> Forest k a -> Forest k a
insTree t Nil = t `Cons` Nil
insTree t1 f@(t2 `Cons` ts)
    | _rank t1 < _rank t2 = t1 `Cons` f
    | otherwise = insTree (link t1 t2) ts

mergeTrees :: Ord k => Forest k a -> Forest k a -> Forest k a
mergeTrees f Nil = f
mergeTrees Nil f = f
mergeTrees f1@(t1 `Cons` ts1) f2@(t2 `Cons` ts2) = case _rank t1 `compare` _rank t2 of
    LT -> t1 `Cons` mergeTrees ts1 f2
    GT -> t2 `Cons` mergeTrees f1 ts2
    EQ -> insTree (link t1 t2) (mergeTrees ts1 ts2)

merge :: Ord k => Forest k a -> Forest k a -> Forest k a
merge f1 f2 = mergeTrees (normalize f1) (normalize f2)
{-# INLINE merge #-}

normalize :: Ord k => Forest k a -> Forest k a
normalize Nil = Nil
normalize (t `Cons` ts) = insTree t ts
{-# INLiNE normalize #-}

ins :: Ord k => k -> a -> Forest k a -> Forest k a
ins key x (t1 `Cons` t2 `Cons` ts)
    | _rank t1 == _rank t2 = key `seq` skewLink key x t1 t2 `Cons` ts
ins key x ts = key `seq` Node 0 key x Nil Nil `Cons` ts

fromForest :: Ord k => Int -> Forest k a -> PrioHeap k a
fromForest _ Nil = Empty
fromForest s f@(_ `Cons` _) =
    let (Node _ key x xs ts1, ts2) = removeMinTree f
    in Heap s key x (foldl' (\acc (Pair key x) -> ins key x acc) (merge (reverse ts1) ts2) xs)

removeMinTree :: Ord k => Forest k a -> (Tree k a, Forest k a)
removeMinTree Nil = error "removeMinTree: empty heap"
removeMinTree (t `Cons` Nil) = (t, Nil)
removeMinTree (t `Cons` ts) =
    let (t', ts') = removeMinTree ts
    in if _root t <= _root t'
        then (t, ts)
        else (t', t `Cons` ts')

instance Show2 PrioHeap where
    liftShowsPrec2 spk slk spv slv p heap = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList heap)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (PrioHeap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
    {-# INLINE liftShowsPrec #-}

instance (Show k, Show a) => Show (PrioHeap k a) where
    showsPrec = showsPrec2
    {-# INLINE showsPrec #-}

instance (Ord k, Read k) => Read1 (PrioHeap k) where
    liftReadsPrec rp rl = readsData $ readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Ord k, Read k, Read a) => Read (PrioHeap k a) where
#ifdef __GLASGOW_HASKELL__
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        pure (fromList xs)
#else
    readsPrec = readsPrec1
    {-# INLINE readPrec #-}
#endif

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

    mappend = (<>)
    {-# INLINE mappend #-}

instance Functor (PrioHeap k) where
    fmap = map
    {-# INLINE fmap #-}

instance Foldable (PrioHeap k) where
    foldMap f = foldMapWithKey (const f)
    {-# INLINE foldMap #-}

    foldr f = foldrWithKey (const f)
    {-# INLINE foldr #-}

    foldl f = foldlWithKey (const . f)
    {-# INLINE foldl #-}

    foldr' f = foldrWithKey' (const f)
    {-# INLINE foldr' #-}

    foldl' f = foldlWithKey' (const . f)
    {-# INLINE foldl' #-}

    null Empty = True
    null Heap{} = False
    {-# INLINE null #-}

    length = size
    {-# INLINE length #-}

instance Traversable (PrioHeap k) where
    traverse f = traverseWithKey (const f)
    {-# INLINE traverse #-}

#ifdef __GLASGOW_HASKELL__
instance Ord k => IsList (PrioHeap k a) where
    type Item (PrioHeap k a) = (k, a)

    fromList = fromList
    {-# INLINE fromList #-}

    toList = toList
    {-# INLINE toList #-}
#endif

instance (NFData k, NFData a) => NFData (PrioHeap k a) where
    rnf Empty = ()
    rnf (Heap _ k x forest) = rnf k `seq` rnf x `seq` rnf forest


-- | /O(1)/. The empty heap.
--
-- > empty = fromList []
empty :: PrioHeap k a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A heap with a single element.
--
-- > singleton x = fromList [x]
singleton :: k -> a -> PrioHeap k a
singleton k x = Heap 1 k x Nil
{-# INLINE singleton #-}

-- | /O(n * log n)/. Create a heap from a list.
fromList :: Ord k => [(k, a)] -> PrioHeap k a
fromList = foldl' (\acc (key, x) -> insert key x acc) empty
{-# INLINE fromList #-}

-- | /O(1)/. Insert a new key and value into the heap.
insert :: Ord k => k -> a -> PrioHeap k a -> PrioHeap k a
insert key x Empty = singleton key x
insert kx x (Heap s ky y f)
    | kx <= ky = Heap (s + 1) kx x (ins ky y f)
    | otherwise = Heap (s + 1) ky y (ins kx x f)

-- | /O(log n)/. The union of two heaps.
union :: Ord k => PrioHeap k a -> PrioHeap k a -> PrioHeap k a
union heap Empty = heap
union Empty heap = heap
union (Heap s1 key1 x1 f1) (Heap s2 key2 x2 f2)
    | key1 <= key2 = Heap (s1 + s2) key1 x1 (ins key2 x2 (merge f1 f2))
    | otherwise = Heap (s1 + s2) key2 x2 (ins key1 x1 (merge f1 f2))

-- | The union of a foldable of heaps.
--
-- > unions = foldl union empty
unions :: (Foldable f, Ord k) => f (PrioHeap k a) -> PrioHeap k a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(n)/. Map a function over the heap.
map :: (a -> b) -> PrioHeap k a -> PrioHeap k b
map f = mapWithKey (const f)
{-# INLINE map #-}

-- | /O(n)/. Map a function that has access to the key associated with a value over the heap.
mapWithKey :: (k -> a -> b) -> PrioHeap k a -> PrioHeap k b
mapWithKey _ Empty = Empty
mapWithKey f (Heap s key x forest) = Heap s key (f key x) (fmap mapTree forest)
  where
    mapTree (Node r key x xs c) = Node r key (f key x) (fmap mapPair xs) (fmap mapTree c)

    mapPair (Pair key x) = Pair key (f key x)

-- | /O(n)/. Traverse the heap with a function that has access to the key associated with a value.
traverseWithKey :: Applicative f => (k -> a -> f b) -> PrioHeap k a -> f (PrioHeap k b)
traverseWithKey f = go
  where
    go Empty = pure Empty
    go (Heap s key x forest) = Heap s key <$> f key x <*> traverse traverseTree forest

    traverseTree (Node r key x xs c) = Node r key <$> f key x <*> traverse traversePair xs <*> traverse traverseTree c
    traversePair (Pair key x) = Pair key <$> f key x
{-# INLINE traverseWithKey #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> PrioHeap k a -> PrioHeap k a
filter f = filterWithKey (const f)
{-# INLINE filter #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> PrioHeap k a
filterWithKey f = foldrWithKey f' empty
  where
    f' key x heap
        | f key x = insert key x heap
        | otherwise = heap
{-# INLINE filterWithKey #-}

-- | /O(n)/. Partition the heap into two heaps, one with all elements that satisfy the predicate
-- and one with all elements that don't satisfy the predicate.
partition :: Ord k => (a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partition f = partitionWithKey (const f)
{-# INLINE partition #-}

-- | /O(n)/. Partition the heap into two heaps, one with all elements that satisfy the predicate
-- and one with all elements that don't satisfy the predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> (PrioHeap k a, PrioHeap k a)
partitionWithKey f = foldrWithKey f' (empty, empty)
  where
    f' key x (heap1, heap2)
        | f key x = (insert key x heap1, heap2)
        | otherwise = (heap1, insert key x heap2)
{-# INLINE partitionWithKey #-}

-- | /O(n)/. Map and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | /O(n)/. Map and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> PrioHeap k a -> PrioHeap k b
mapMaybeWithKey f = foldrWithKey f' empty
  where
    f' key x heap = case f key x of
        Just y -> insert key y heap
        Nothing -> heap
{-# INLINE mapMaybeWithKey #-}

-- | /O(n)/. Map and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEither f = mapEitherWithKey (const f)
{-# INLINE mapEither #-}

-- | /O(n)/. Map and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> PrioHeap k a -> (PrioHeap k b, PrioHeap k c)
mapEitherWithKey f = foldrWithKey f' (empty, empty)
  where
    f' key x (heap1, heap2) = case f key x of
        Left y -> (insert key y heap1, heap2)
        Right y -> (heap1, insert key y heap2)
{-# INLINE mapEitherWithKey #-}

-- | /O(n)/. Fold the keys and values in the heap, using the given monoid.
foldMapWithKey :: Monoid m => (k -> a -> m) -> PrioHeap k a -> m
foldMapWithKey f = foldrWithKey (\key x acc -> f key x `mappend` acc) mempty
{-# INLINE foldMapWithKey #-}

-- | /O(n)/. Fold the keys and values in the heap, using the given right-associative function.
foldrWithKey :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey f acc = go
  where
    go Empty = acc
    go (Heap _ key x forest) = f key x (foldr foldTree acc forest)

    foldTree (Node _ key x xs c) acc = f key x (foldr (uncurry f) (foldr foldTree acc c) xs)
{-# INLINE foldrWithKey #-}

-- | /O(n)/. Fold the keys and values in the heap, using the given left-associative function.
foldlWithKey :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey f acc = go
  where
    go Empty = acc
    go (Heap _ key x forest) = foldl foldTree (f acc key x) forest

    foldTree acc (Node _ key x xs c) = foldl foldTree (foldl (uncurry . f) (f acc key x) xs) c
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'.
-- Each application of the function is evaluated before using the result in the next application.
foldrWithKey' :: (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKey' f acc h = foldlWithKey f' id h acc
  where
    f' k key x z = k $! f key x z
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. A strict version of 'foldlWithKey'.
-- Each application of the function is evaluated before using the result in the next application.
foldlWithKey' :: (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKey' f acc h = foldrWithKey f' id h acc
  where
    f' key x k z = k $! f z key x
{-# INLINE foldlWithKey' #-}

-- | /O(n * log n)/. Fold the values in the heap in order, using the given monoid.
foldMapOrd :: (Ord k, Monoid m) => (a -> m) -> PrioHeap k a -> m
foldMapOrd f = foldMapWithKeyOrd (const f)
{-# INLINE foldMapOrd #-}

-- | /O(n * log n)/. Fold the values in the heap in order, using the given right-associative function.
foldrOrd :: Ord k => (a -> b -> b) -> b -> PrioHeap k a -> b
foldrOrd f = foldrWithKeyOrd (const f)
{-# INLINE foldrOrd #-}

-- | /O(n * log n)/. Fold the values in the heap in order, using the given left-associative function.
foldlOrd :: Ord k => (b -> a -> b) -> b -> PrioHeap k a -> b
foldlOrd f = foldlWithKeyOrd (const . f)
{-# INLINE foldlOrd #-}

-- | /O(n * log n)/. A strict version of 'foldrOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldrOrd' :: Ord k => (a -> b -> b) -> b -> PrioHeap k a -> b
foldrOrd' f = foldrWithKeyOrd' (const f)
{-# INLINE foldrOrd' #-}

-- | /O(n)/. A strict version of 'foldlOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldlOrd' :: Ord k => (b -> a -> b) -> b -> PrioHeap k a -> b
foldlOrd' f = foldlWithKeyOrd' (const . f)
{-# INLINE foldlOrd' #-}

-- | /O(n * log n)/. Fold the keys and values in the heap in order, using the given monoid.
foldMapWithKeyOrd :: (Ord k, Monoid m) => (k -> a -> m) -> PrioHeap k a -> m
foldMapWithKeyOrd f = foldrWithKeyOrd (\key x acc -> f key x `mappend` acc) mempty
{-# INLINE foldMapWithKeyOrd #-}

-- | /O(n * log n)/. Fold the keys and values in the heap in order, using the given right-associative function.
foldrWithKeyOrd :: Ord k => (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKeyOrd f acc = go
  where
    go h = case minView h of
        Nothing -> acc
        Just ((key, x), h') -> f key x (go h')
{-# INLINE foldrWithKeyOrd #-}

-- | /O(n * log n)/. Fold the keys and values in the heap in order, using the given left-associative function.
foldlWithKeyOrd :: Ord k => (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKeyOrd f = go
  where
    go acc h = case minView h of
        Nothing -> acc
        Just ((key, x), h') -> go (f acc key x) h'
{-# INLINE foldlWithKeyOrd #-}

-- | /O(n * log n)/. A strict version of 'foldrWithKeyOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldrWithKeyOrd' :: Ord k => (k -> a -> b -> b) -> b -> PrioHeap k a -> b
foldrWithKeyOrd' f acc h = foldlWithKeyOrd f' id h acc
  where
    f' k key x z = k $! f key x z
{-# INLINE foldrWithKeyOrd' #-}

-- | /O(n)/. A strict version of 'foldlWithKeyOrd'.
-- Each application of the function is evaluated before using the result in the next application.
foldlWithKeyOrd' :: Ord k => (b -> k -> a -> b) -> b -> PrioHeap k a -> b
foldlWithKeyOrd' f acc h = foldrWithKeyOrd f' id h acc
  where
    f' key x k z = k $! f z key x
{-# INLINE foldlWithKeyOrd' #-}

-- | /O(1)/. The number of elements in the heap.
size :: PrioHeap k a -> Int
size Empty = 0
size (Heap s _ _ _) = s
{-# INLINE size #-}

-- | /O(n)/. Is the key a member of the heap?
member :: Ord k => k -> PrioHeap k a -> Bool
member _ Empty = False
member kx (Heap _ ky _ forest) = kx <= ky && any (kx `elemTree`) forest
  where
    kx `elemTree` (Node _ ky _ ys c) = kx <= ky && (any (\(Pair a _) -> kx == a) ys || any (kx `elemTree`) c)

-- | /O(n)/. Is the value not a member of the heap?
notMember :: Ord k => k -> PrioHeap k a -> Bool
notMember key = not . member key

-- | /O(1)/. Adjust the value at the minimal key.
adjustMin :: (a -> a) -> PrioHeap k a -> PrioHeap k a
adjustMin f = adjustMinWithKey (const f)
{-# INLINE adjustMin #-}

-- | /O(1)/. Adjust the value at the minimal key.
adjustMinWithKey :: (k -> a -> a) -> PrioHeap k a -> PrioHeap k a
adjustMinWithKey _ Empty = Empty
adjustMinWithKey f (Heap s key x forest) = Heap s key (f key x) forest

-- | /O(1)/. The minimal element in the heap or 'Nothing' if the heap is empty.
lookupMin :: PrioHeap k a -> Maybe (k, a)
lookupMin Empty = Nothing
lookupMin (Heap _ key x _) = Just (key, x)
{-# INLINE lookupMin #-}

-- | /O(1)/. The minimal element in the heap. Calls 'error' if the heap is empty.
findMin :: PrioHeap k a -> (k, a)
findMin heap = fromMaybe (errorEmpty "findMin") (lookupMin heap)
{-# INLINE findMin #-}

-- | /O(log n)/. Delete the minimal element. Returns the empty heap if the heap is empty.
deleteMin :: Ord k => PrioHeap k a -> PrioHeap k a
deleteMin Empty = Empty
deleteMin (Heap s _ _ f) = fromForest (s - 1) f

-- | /O(log n)/. Delete and find the minimal element. Calls 'error' if the heap is empty.
--
-- > deleteFindMin heap = (findMin heap, deleteMin heap)
deleteFindMin :: Ord k => PrioHeap k a -> ((k, a), PrioHeap k a)
deleteFindMin heap = fromMaybe (errorEmpty "deleteFindMin") (minView heap)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Update the value at the minimal key.
updateMin :: Ord k => (a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMin f = updateMinWithKey (const f)
{-# INLINE updateMin #-}

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> PrioHeap k a -> PrioHeap k a
updateMinWithKey _ Empty = Empty
updateMinWithKey f (Heap s key x forest) = case f key x of
    Nothing -> fromForest (s - 1) forest
    Just x' -> Heap s key x' forest

-- | /O(log n)/. Retrieves the minimal key/value pair of the heap and the heap stripped of that element or 'Nothing' if the heap is empty.
minView :: Ord k => PrioHeap k a -> Maybe ((k, a), PrioHeap k a)
minView Empty = Nothing
minView (Heap s key x f) = Just ((key, x), fromForest (s - 1) f)
{-# INLINE minView #-}

-- | /O(n * log n)/. @take n heap@ takes the @n@ smallest elements of @heap@, in ascending order.
--
-- > take n heap = take n (toAscList heap)
take :: Ord k => Int -> PrioHeap k a -> [(k, a)]
take n h
    | n <= 0 = []
    | otherwise = case minView h of
        Nothing -> []
        Just (x, h') -> x : take (n - 1) h'

-- | /O(n * log n)/. @drop n heap@ drops the @n@ smallest elements from @heap@.
drop :: Ord k => Int -> PrioHeap k a -> PrioHeap k a
drop n h
    | n <= 0 = h
    | otherwise = drop (n - 1) (deleteMin h)

-- | /O(n * log n)/. @splitAt n heap@ takes and drops the @n@ smallest elements from @heap@.
splitAt :: Ord k => Int -> PrioHeap k a -> ([(k, a)], PrioHeap k a)
splitAt n h
    | n <= 0 = ([], h)
    | otherwise = case minView h of
        Nothing -> ([], h)
        Just (x, h') -> let (xs, h'') = splitAt (n - 1) h' in (x : xs, h'')

-- | /O(n * log n)/. @takeWhile p heap@ takes the elements from @heap@ in ascending order, while @p@ holds.
takeWhile :: Ord k => (a -> Bool) -> PrioHeap k a -> [(k, a)]
takeWhile p = takeWhileWithKey (const p)
{-# INLINE takeWhile #-}

-- | /O(n * log n)/. @takeWhileWithKey p heap@ takes the elements from @heap@ in ascending order, while @p@ holds.
takeWhileWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> [(k, a)]
takeWhileWithKey p = go
  where
    go h = case minView h of
        Nothing -> []
        Just ((key, x), h') -> if p key x then (key, x) : go h' else []
{-# INLINE takeWhileWithKey #-}

-- | /O(n * log n)/. @dropWhile p heap@ drops the elements from @heap@ in ascending order, while @p@ holds.
dropWhile :: Ord k => (a -> Bool) -> PrioHeap k a -> PrioHeap k a
dropWhile p = dropWhileWithKey (const p)
{-# INLINE dropWhile #-}

-- | /O(n * log n)/. @dropWhileWithKey p heap@ drops the elements from @heap@ in ascending order, while @p@ holds.
dropWhileWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> PrioHeap k a
dropWhileWithKey p = go
  where
    go h = case minView h of
        Nothing -> h
        Just ((key, x), h') -> if p key x then go h' else h
{-# INLINE dropWhileWithKey #-}

-- | /O(n * log n)/. @span p heap@ takes and drops the elements from @heap@, while @p@ holds
span :: Ord k => (a -> Bool) -> PrioHeap k a -> ([(k, a)], PrioHeap k a)
span p = spanWithKey (const p)
{-# INLINE span #-}

-- | /O(n * log n)/. @spanWithKey p heap@ takes and drops the elements from @heap@, while @p@ holds
spanWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> ([(k, a)], PrioHeap k a)
spanWithKey p = go
  where
    go h = case minView h of
        Nothing -> ([], h)
        Just ((key, x), h') -> if p key x
            then let (xs, h'') = go h' in ((key, x) : xs, h'')
            else ([], h)
{-# INLINE spanWithKey #-}

-- | /O(n * log n)/. @span@, but with inverted predicate.
break :: Ord k => (a -> Bool) -> PrioHeap k a -> ([(k, a)], PrioHeap k a)
break p = span (not . p)
{-# INLINE break #-}

-- | /O(n * log n)/. @spanWithKey@, but with inverted predicate.
breakWithKey :: Ord k => (k -> a -> Bool) -> PrioHeap k a -> ([(k, a)], PrioHeap k a)
breakWithKey p = spanWithKey (\key x -> not (p key x))
{-# INLINE breakWithKey #-}

-- | /O(n * log n)/. Remove duplicate elements from the heap.
nub :: Ord k => PrioHeap k a -> PrioHeap k a
nub h = case minView h of
    Nothing -> Empty
    Just ((key, x), h') -> insert key x (nub (dropWhileWithKey (const . (== key)) h'))

-- | /O(n)/. Create a list of key/value pairs from the heap.
toList :: PrioHeap k a -> [(k, a)]
toList = foldrWithKey (\key x acc -> (key, x) : acc) []

-- | /O(n * log n)/. Create an ascending list of key/value pairs from the heap.
toAscList :: Ord k => PrioHeap k a -> [(k, a)]
toAscList = foldrWithKeyOrd (\key x acc -> (key, x) : acc) []

-- | /O(n * log n)/. Create a descending list of key/value pairs from the heap.
toDescList :: Ord k => PrioHeap k a -> [(k, a)]
toDescList = foldlWithKeyOrd (\acc key x -> (key, x) : acc) []

-- | /O(n)/. Create a heap from a 'Data.Heap.Heap' of keys and a function which computes the value for each key.
fromHeap :: (k -> a) -> Heap.Heap k -> PrioHeap k a
fromHeap _ Heap.Empty = Empty
fromHeap f (Heap.Heap s key forest) = Heap s key (f key) (fmap fromTree forest)
  where
    fromTree (Heap.Node r key xs c) = Node r key (f key) (fmap (\key -> Pair key (f key)) xs) (fmap fromTree c)

-- | Create a 'Data.Heap.Heap' of all keys of the heap
keysHeap :: PrioHeap k a -> Heap.Heap k
keysHeap Empty = Heap.Empty
keysHeap (Heap s key _ forest) = Heap.Heap s key (fmap fromTree forest)
  where
    fromTree (Node r key _ xs c) = Heap.Node r key (fmap (\(Pair key _) -> key) xs) (fmap fromTree c)
