{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeFamilies #-}
#endif

{- |
= Finite vectors

The @'Vector' a@ type represents a finite vector (or dynamic array) of elements of type @a@.
A 'Vector' is strict in its spine.

The class instances are based on those for lists.

This module should be imported qualified, to avoid name clashes with the 'Prelude'.

== Performance

The worst case running time complexities are given, with /n/ referring the the number of elements in the vector.
A 'Vector' is particularly efficient for applications that require a lot of indexing and updates.
All logarithms are base 16, which means that /O(log n)/ behaves more like /O(1)/ in practice.

For a similar container with efficient concatenation and splitting, but slower indexing and updates,
see [Seq](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html) from the
[containers](https://hackage.haskell.org/package/containers) package.

== Warning

The length of a 'Vector' must not exceed @'maxBound' :: 'Int'@.
Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the vector is undefined.

== Implementation

The implementation of 'Vector' uses array mapped tries. For a good explanation,
see [this blog post](https://hypirion.com/musings/understanding-persistent-vector-pt-1).
-}

-- TODO: document unsafe

module Data.AMT
    ( Vector
    -- * Construction
    , empty, singleton, fromList
    , fromFunction
    , replicate, replicateA
    , unfoldr, unfoldl, iterateN
    , (<|), (|>), (><)
    -- * Deconstruction/Subranges
    , viewl, viewr
    , head, last
    , take
    -- * Indexing
    , lookup, index
    , (!?), (!)
    , update
    , adjust
    -- * Transformations
    , map, mapWithIndex
    , traverseWithIndex
    , indexed
    -- * Folds
    , foldMapWithIndex
    , foldlWithIndex, foldrWithIndex
    , foldlWithIndex', foldrWithIndex'
    -- * Zipping/Unzipping
    , zip, zipWith
    , zip3, zipWith3
    , unzip, unzip3
    -- * To Lists
    , toIndexedList
    ) where

import Control.Applicative (Alternative)
import qualified Control.Applicative as Applicative
import Control.Monad (MonadPlus(..))
#if !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Zip (MonadZip(..))

import Data.Bits
import Data.Foldable (foldl', toList)
import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty(..), (!!))
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
#ifdef __GLASGOW_HASKELL__
import Data.String (IsString)
#endif
import Data.Traversable (mapAccumL)
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
#endif
import Prelude hiding ((!!), head, last, lookup, map, replicate, tail, take, unzip, unzip3, zip, zipWith, zip3, zipWith3)
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec)

import Control.DeepSeq (NFData(..))

import qualified Util.Internal.Array as A
import Util.Internal.Indexed (Indexed(..), evalIndexed)

infixr 5 ><
infixr 5 <|
infixl 5 |>

data Tree a
    = Internal !(A.Array (Tree a))
    | Leaf !(A.Array a)

-- | An array mapped trie.
data Vector a
    = Empty
    | Root
        {-# UNPACK #-} !Int  -- ^ size
        {-# UNPACK #-} !Int  -- ^ offset (number of elements in the tree)
        {-# UNPACK #-} !Int  -- ^ height (of the tree)
        !(Tree a)  -- ^ tree
        !(NonEmpty a)  -- ^ tail (reversed)

instance NFData a => NFData (Tree a) where
    rnf (Internal v) = rnf v
    rnf (Leaf v) = rnf v

errorNegativeLength :: String -> a
errorNegativeLength s = error $ "AMT." ++ s ++ ": expected a nonnegative length"

-- | The number of bits used per level.
bits :: Int
bits = 4
{-# INLINE bits #-}

-- | The maximum size of the tail.
tailSize :: Int
tailSize = 1 `shiftL` bits

-- | The mask used to extract the index into the array.
mask :: Int
mask = tailSize - 1

instance Show1 Vector where
    liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance Show a => Show (Vector a) where
    showsPrec = showsPrec1
    {-# INLINE showsPrec #-}

instance Read1 Vector where
    liftReadsPrec rp rl = readsData $ readsUnaryWith (liftReadsPrec rp rl) "fromList" fromList

instance Read a => Read (Vector a) where
#ifdef __GLASGOW_HASKELL__
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        pure (fromList xs)
#else
    readsPrec = readsPrec1
#endif

instance Eq1 Vector where
    liftEq f v1 v2 = length v1 == length v2 && liftEq f (toList v1) (toList v2)

instance Eq a => Eq (Vector a) where
    (==) = eq1

instance Ord1 Vector where
    liftCompare f v1 v2 = liftCompare f (toList v1) (toList v2)

instance Ord a => Ord (Vector a) where
    compare = compare1

instance Semigroup (Vector a) where
    (<>) = (><)

instance Monoid (Vector a) where
    mempty = empty

    mappend = (<>)

instance Foldable Vector where
    foldr f acc = go
      where
        go Empty = acc
        go (Root _ _ _ tree tail) = foldrTree tree (foldr f acc (L.reverse tail))

        foldrTree (Internal v) acc' = foldr foldrTree acc' v
        foldrTree (Leaf v) acc' = foldr f acc' v
    {-# INLINE foldr #-}

    null Empty = True
    null Root{} = False
    {-# INLINE null #-}

    length Empty = 0
    length (Root s _ _ _ _) = s
    {-# INLINE length #-}

instance Functor Vector where
    fmap = map

instance Traversable Vector where
    traverse f = go
      where
        go Empty = pure empty
        go (Root s offset h tree (x :| tail)) =
            Root s offset h <$> traverseTree tree <*> (flip (:|) <$> traverseReverse tail <*> f x)

        traverseReverse [] = pure []
        traverseReverse (x : xs) = flip (:) <$> traverseReverse xs <*> f x

        traverseTree (Internal v) = Internal <$> traverse traverseTree v
        traverseTree (Leaf v) = Leaf <$> traverse f v
    {-# INLINE traverse #-}

#ifdef __GLASGOW_HASKELL__
instance IsList (Vector a) where
    type Item (Vector a) = a

    fromList = fromList

    toList = toList

instance a ~ Char => IsString (Vector a) where
    fromString = fromList
#endif

instance Applicative Vector where
    pure = singleton

    fs <*> xs = foldl' (\acc f -> acc >< map f xs) empty fs

instance Monad Vector where
    xs >>= f = foldl' (\acc x -> acc >< f x) empty xs

instance Alternative Vector where
    empty = empty

    (<|>) = (><)

instance MonadPlus Vector

instance MonadFail Vector where
    fail _ = empty

instance MonadZip Vector where
    mzip = zip

    mzipWith = zipWith

    munzip = unzip

instance NFData a => NFData (Vector a) where
    rnf Empty = ()
    rnf (Root _ _ _ tree tail) = rnf tree `seq` rnf tail


-- | /O(1)/. The empty vector.
--
-- > empty = fromList []
empty :: Vector a
empty = Empty

-- | /O(1)/. A vector with a single element.
--
-- > singleton x = fromList [x]
singleton :: a -> Vector a
singleton x = Root 1 0 0 (Leaf A.empty) (x :| [])

-- | /O(n * log n)/. Create a new vector from a list.
fromList :: [a] -> Vector a
fromList = foldl' (|>) empty

-- | Create a new vector of the given length from a function.
fromFunction :: Int -> (Int -> a) -> Vector a
fromFunction n f = if n < 0 then errorNegativeLength "fromFunction" else go 0 empty
  where
    go i acc
        | i < n = go (i + 1) (acc |> f i)
        | otherwise = acc

-- | /O(n * log n)/. @replicate n x@ is a vector consisting of n copies of x.
replicate :: Int -> a -> Vector a
replicate n x = if n < 0 then errorNegativeLength "replicate" else go 0 empty
  where
    go i acc
        | i < n = go (i + 1) (acc |> x)
        | otherwise = acc

-- | @replicateA@ is an 'Applicative' version of 'replicate'.
replicateA :: Applicative f => Int -> f a -> f (Vector a)
replicateA n x = if n < 0 then errorNegativeLength "replicateA" else go 0 (pure empty)
  where
    go i acc
        | i < n = go (i + 1) ((|>) <$> acc <*> x)
        | otherwise = acc

-- | /O(n * log n)/. Build a vector from left to right by repeatedly applying a function to a seed value.
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr f = go empty
  where
    go v acc = case f acc of
        Nothing -> v
        Just (x, acc') -> go (v |> x) acc'
{-# INLINE unfoldr #-}

-- | /O(n * log n)/. Build a vector from right to left by repeatedly applying a function to a seed value.
unfoldl :: (b -> Maybe (b, a)) -> b -> Vector a
unfoldl f = go
  where
    go acc = case f acc of
        Nothing -> empty
        Just (acc', x) -> go acc' |> x
{-# INLINE unfoldl #-}

-- | Constructs a vector by repeatedly applying a function to a seed value.
iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN n f x = if n < 0 then errorNegativeLength "iterateN" else go 0 x empty
  where
    go i y acc
        | i < n = go (i + 1) (f y) (acc |> y)
        | otherwise = acc

-- | /O(n * log n)/. Add an element to the left end of the vector.
(<|) :: a -> Vector a -> Vector a
x <| v = fromList $ x : toList v

-- | /O(n * log n)/. The first element and the vector without the first element or 'Nothing' if the vector is empty.
viewl :: Vector a -> Maybe (a, Vector a)
viewl v = case toList v of
    [] -> Nothing
    x : xs -> Just (x, fromList xs)

-- | /O(log n)/. Add an element to the right end of the vector.
(|>) :: Vector a -> a -> Vector a
Empty |> x = singleton x
Root s offset h tree tail |> x
    | s .&. mask /= 0 = Root (s + 1) offset h tree (x L.<| tail)
    | offset == 0 = Root (s + 1) s h (Leaf $ A.fromTail tailSize tail) (x :| [])
    | offset == 1 `shiftL` (bits * (h + 1)) = Root (s + 1) s (h + 1) (Internal $ A.fromList2 tree (newPath h)) (x :| [])
    | otherwise = Root (s + 1) s h (insertTail (bits * h) tree) (x :| [])
  where
    -- create a new path from the old tail
    newPath 0 = Leaf $ A.fromTail tailSize tail
    newPath h = Internal $ A.singleton (newPath (h - 1))

    insertTail sh (Internal v)
        | idx < length v = Internal $ A.adjust idx (insertTail (sh - bits)) v
        | otherwise = Internal $ A.snoc v (newPath (sh `div` bits - 1))
      where
        idx = offset `shiftR` sh .&. mask
    insertTail _ (Leaf _) = Leaf $ A.fromTail tailSize tail

-- | /O(log n)/. The vector without the last element and the last element or 'Nothing' if the vector is empty.
viewr :: Vector a -> Maybe (Vector a, a)
viewr Empty = Nothing
viewr (Root s offset h tree (x :| tail))
    | not (null tail) = Just (Root (s - 1) offset h tree (L.fromList tail), x)
    | s == 1 = Just (Empty, x)
    | s == tailSize + 1 = Just (Root (s - 1) 0 0 (Leaf A.empty) (getTail tree), x)
    | otherwise = Just (normalize $ Root (s - 1) (offset - tailSize) h (unsnocTree (bits * h) tree) (getTail tree), x)
  where
    idx = offset - tailSize - 1

    unsnocTree sh (Internal v) =
        let subIndex = idx `shiftR` sh .&. mask
            new = A.take (subIndex + 1) v
        in Internal $ A.adjust subIndex (unsnocTree (sh - bits)) new
    unsnocTree _ (Leaf v) = Leaf v

    getTail (Internal v) = getTail (A.last v)
    getTail (Leaf v) = A.toTail v

    normalize (Root s offset h (Internal v) tail)
        | length v == 1 = Root s offset (h - 1) (A.head v) tail
    normalize v = v

-- | /O(log n)/. The first element in the vector or 'Nothing' if the vector is empty.
head :: Vector a -> Maybe a
head Empty = Nothing
head (Root _ 0 _ _ tail) = Just (L.last tail) -- offset 0, all elements are in the tail
head (Root _ _ _ tree _) = Just (headTree tree)
  where
    headTree (Internal v) = headTree (A.head v)
    headTree (Leaf v) = A.head v

-- | /O(1)/. The last element in the vector or 'Nothing' if the vector is empty.
last :: Vector a -> Maybe a
last Empty = Nothing
last (Root _ _ _ _ (x :| _)) = Just x

-- | /O(log n)/. Take the first n elements of the vector or the vector if n is larger than the length of the vector.
-- Returns the empty vector if n is negative.
take :: Int -> Vector a -> Vector a
take _ Empty = Empty
take n root@(Root s offset h tree tail)
    | n <= 0 = Empty
    | n >= s = root
    | n > offset = Root n offset h tree (L.fromList $ L.drop (s - n) tail)
    | n <= tailSize = Root n 0 0 (Leaf A.empty) (getTail (bits * h) tree)
    | otherwise =
        let sh = bits * h
        in normalize $ Root n ((n - 1) .&. complement mask) h (takeTree sh tree) (getTail sh tree)  -- n - 1 because if 'n .&. mask == 0', we need to subtract tailSize
  where
    -- index of the last element in the new vector
    idx = n - 1

    idx' = idx - tailSize

    takeTree sh (Internal v) =
        let subIndex = idx' `shiftR` sh .&. mask
            new = A.take (subIndex + 1) v
        in Internal $ A.adjust subIndex (takeTree (sh - bits)) new
    takeTree _ (Leaf v) = Leaf v

    getTail sh (Internal v) = getTail (sh - bits) (A.index (idx `shiftR` sh .&. mask) v)
    getTail _ (Leaf v) = A.toTail $ A.take (idx .&. mask + 1) v

    normalize (Root s offset h (Internal v) tail)
        | length v == 1 = normalize $ Root s offset (h - 1) (A.head v) tail
    normalize v = v

-- | /O(log n)/. The element at the index or 'Nothing' if the index is out of range.
lookup :: Int -> Vector a -> Maybe a
lookup _ Empty = Nothing
lookup i (Root s offset h tree tail)
    | i < 0 || i >= s = Nothing  -- index out of range
    | i < offset = Just $ lookupTree (bits * h) tree
    | otherwise = Just $ tail !! (s - i - 1)
  where
    lookupTree sh (Internal v) = lookupTree (sh - bits) (A.index (i `shiftR` sh .&. mask) v)
    lookupTree _ (Leaf v) = A.index (i .&. mask) v

-- | /O(log n)/. The element at the index. Calls 'error' if the index is out of range.
index :: Int -> Vector a -> a
index i = fromMaybe (error "AMT.index: index out of range") . lookup i

-- | /O(log n)/. Flipped version of 'lookup'.
(!?) :: Vector a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | /O(log n)/. Flipped version of 'index'.
(!) :: Vector a -> Int -> a
(!) = flip index
{-# INLINE (!) #-}

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
    | i < 0 || i >= s = root  -- index out of range
    | i < offset = Root s offset h (adjustTree (bits * h) tree) tail
    | otherwise = let (l, x : r) = L.splitAt (s - i - 1) tail in Root s offset h tree (L.fromList $ l ++ (f x : r))
  where
    adjustTree sh (Internal v) =
        let idx = i `shiftR` sh .&. mask
        in Internal $ A.adjust idx (adjustTree (sh - bits)) v
    adjustTree _ (Leaf v) =
        let idx = i .&. mask
        in Leaf $ A.adjust idx f v

-- | /O(m * log n)/. Concatenate two vectors.
(><) :: Vector a -> Vector a -> Vector a
Empty >< v = v
v >< Empty = v
v1 >< v2 = foldl' (|>) v1 v2
{-# INLINE (><) #-}

-- | /O(n)/. Map a function over the vector.
map :: (a -> b) -> Vector a -> Vector b
map _ Empty = Empty
map f (Root s offset h tree tail) = Root s offset h (mapTree tree) (fmap f tail)
  where
    mapTree (Internal v) = Internal (fmap mapTree v)
    mapTree (Leaf v) = Leaf (fmap f v)

-- | /O(n)/. Map a function that has access to the index of an element over the vector.
mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f = snd . mapAccumL (\i x -> i `seq` (i + 1, f i x)) 0
{-# INLINE mapWithIndex #-}

-- | /O(n)/. Fold the values in the vector, using the given monoid.
foldMapWithIndex :: Monoid m => (Int -> a -> m) -> Vector a -> m
foldMapWithIndex f = foldrWithIndex (\i -> mappend . f i) mempty
{-# INLINE foldMapWithIndex #-}

-- | /O(n)/. Fold using the given left-associative function that has access to the index of an element.
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Vector a -> b
foldlWithIndex f acc v = foldl f' (const acc) v (length v - 1)
  where
    f' g x i = i `seq` f (g (i - 1)) i x
{-# INLINE foldlWithIndex #-}

-- | /O(n)/. Fold using the given right-associative function that has access to the index of an element.
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Vector a -> b
foldrWithIndex f acc v = foldr f' (const acc) v 0
  where
    f' x g i = i `seq` f i x (g (i + 1))
{-# INLINE foldrWithIndex #-}

-- | /O(n)/. A strict version of 'foldlWithIndex'.
-- Each application of the function is evaluated before using the result in the next application.
foldlWithIndex' :: (b -> Int -> a -> b) -> b -> Vector a -> b
foldlWithIndex' f acc v = foldrWithIndex f' id v acc
  where
    f' i x k z = k $! f z i x
{-# INLINE foldlWithIndex' #-}

-- | /O(n)/. A strict version of 'foldrWithIndex'.
-- Each application of the function is evaluated before using the result in the next application.
foldrWithIndex' :: (Int -> a -> b -> b) -> b -> Vector a -> b
foldrWithIndex' f acc v = foldlWithIndex f' id v acc
  where
    f' k i x z = k $! f i x z
{-# INLINE foldrWithIndex' #-}

-- | /O(n)/. Traverse the vector with a function that has access to the index of an element.
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Vector a -> f (Vector b)
traverseWithIndex f v = evalIndexed (traverse (Indexed . f') v) 0
  where
    f' x i = i `seq` (f i x, i + 1)
{-# INLINE traverseWithIndex #-}

-- | /O(n)/. Pair each element in the vector with its index.
indexed :: Vector a -> Vector (Int, a)
indexed = mapWithIndex (,)
{-# INLINE indexed #-}

-- | /O(n)/. Takes two vectors and returns a vector of corresponding pairs.
zip :: Vector a -> Vector b -> Vector (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

-- | /O(n)/. A generalized 'zip' zipping with a function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f v1 v2
    | length v1 >= length v2 = snd $ mapAccumL f' (toList v1) v2
    | otherwise = zipWith (flip f) v2 v1
  where
    f' [] _ = error "unreachable"
    f' (x : xs) y = (xs, f x y)

-- | /O(n)/. Takes three vectors and returns a vector of corresponding triples.
zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

-- | /O(n)/. A generalized 'zip3' zipping with a function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 f v1 v2 v3 = zipWith ($) (zipWith f v1 v2) v3

-- | /O(n)/. Transforms a vector of pairs into a vector of first components and a vector of second components.
unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip v = (map fst v, map snd v)

-- | /O(n)/. Takes a vector of triples and returns three vectors, analogous to 'unzip'.
unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 v = (map fst3 v, map snd3 v, map trd3 v)
  where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    trd3 (_, _, z) = z

-- | /O(n)/. Create a list of index-value pairs from the vector.
toIndexedList :: Vector a -> [(Int, a)]
toIndexedList = foldrWithIndex (curry (:)) []
