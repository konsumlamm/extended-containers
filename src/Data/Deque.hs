{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- TODO:
    fromFunction, replicate, replicateA, iterateN?
    take, drop, splitAt?
-}

{- |
= Finite deques

The @'Deque' a@ type represents a finite deque (double ended queue) of elements of type @a@.

== Pattern Synonyms

Deques can be constructed and matched using the 'Empty', ':<|', and ':|>' pattern synonyms.

== Performance

The worst case running time complexities are given, with /n/ referring the the number of elements in the deque.

== Comparison to [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html)

'Deque' and [Seq](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html#t:Seq) both provide /O(1)/ deque operations (@<|@, @|>@, @viewl@ and @viewr@),
however these are only amortized bounds for @Seq@, while they are worst case bounds for @Deque@.
Moreover, @Deque@ provides /O(1)/ 'reverse'.

== Warning

The length of a 'Deque' must not exceed @'maxBound' :: 'Int'@.
Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the heap is undefined.

== Implementation

The implementation used is described in

* Chris Okasaki, [\"Simple and Efficient Purely Functional Queues and Deques\"](http://citeseerx.ist.psu.edu/viewdoc/summary;jsessionid=956CDC491C58DEDC24DBDC238E1A41FF?doi=10.1.1.47.8825), 1998
-}

-- TODO: remove some functions?
-- TODO: use only pattern synonyms?
-- TODO: remove unnecessary INLINEs

module Data.Deque
    ( Deque(Empty, (:<|), (:|>))
    -- * Construction
    , empty
    , singleton
    , fromList
    , unfoldr, unfoldl
    , (<|), (|>)
    , (><)
    -- * Deconstruction
    , ViewL(..), viewl
    , ViewR(..), viewr
    -- * Transformations
    , reverse
    , map
    -- * Filter
    , filter, partition
    , mapMaybe, mapEither
    -- * Subranges
    , takeWhileL, takeWhileR
    , dropWhileL, dropWhileR
    , spanl, spanr
    , breakl, breakr
    -- * Zipping/Unzipping
    , zip, zipWith
    , zip3, zipWith3
    , unzip, unzip3
    ) where

import Control.Applicative (Alternative)
import qualified Control.Applicative as Applicative
import Control.Monad (MonadPlus(..))
#if !(MIN_VERSION_base(4,13,0))
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Zip (MonadZip(..))

import Data.Foldable (foldl', foldr', toList)
import Data.Functor.Classes
import qualified Data.List as List
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
import Prelude hiding (filter, map, reverse, tail, unzip, unzip3, zip, zipWith, zip3, zipWith3)
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec)

import Control.DeepSeq

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

infixr 5 :<|
infixl 5 :|>

{-# COMPLETE (:<|), Empty #-}
{-# COMPLETE (:|>), Empty #-}

pattern Empty :: Deque a
pattern Empty = Deque 0 [] [] 0 [] []

pattern (:<|) :: a -> Deque a -> Deque a
pattern x :<| xs <- (viewl -> x :< xs)
  where
    x :<| xs = x <| xs

pattern (:|>) :: Deque a -> a -> Deque a
pattern xs :|> x <- (viewr -> xs :> x)
  where
    x :|> xs = x |> xs

tail :: [a] -> [a]
tail [] = []
tail (_ : xs) = xs

-- Invariant: @length ls <= 3 * length rs + 1 && 3 * length ls + 1 >= length rs)@.
data Deque a = Deque !Int [a] [a] !Int [a] [a]

-- Restores the invariant.
check :: Deque a -> Deque a
check d@(Deque ll ls _ rl rs _)
    | ll > 3 * rl + 1 =
        let size = ll + rl
            n = size `div` 2
            ls'' = take n ls
            rs'' = rot1 n rs ls
        in Deque n ls'' ls'' (size - n) rs'' rs''
    | rl > 3 * ll + 1 =
        let size = ll + rl
            n = size `div` 2
            ls'' = rot1 n ls rs
            rs'' = take n rs
        in Deque (size - n) ls'' ls'' n rs'' rs''
    | otherwise = d
  where
    rot1 n ls rs
        | n >= 3 = head ls : rot1 (n - 3) (tail ls) (drop 3 rs)
        | otherwise = rot2 ls (drop n rs) []

    rot2 [] rs xs = List.reverse rs ++ xs
    rot2 (l : ls) rs xs = l : rot2 ls (drop 3 rs) (List.reverse (take 3 rs) ++ xs)

instance Show1 Deque where
    liftShowsPrec sp sl p v = showsUnaryWith (liftShowsPrec sp sl) "fromList" p (toList v)

instance Show a => Show (Deque a) where
    showsPrec = showsPrec1
    {-# INLINE showsPrec #-}

instance Read1 Deque where
    liftReadsPrec rp rl = readsData $ readsUnaryWith (liftReadsPrec rp rl) "fromList" fromList

instance Read a => Read (Deque a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        pure (fromList xs)

instance Eq1 Deque where
    liftEq f d1 d2 = length d1 == length d2 && liftEq f (toList d1) (toList d2)

instance Eq a => Eq (Deque a) where
    (==) = eq1
    {-# INLINE (==) #-}

instance Ord1 Deque where
    liftCompare f d1 d2 = liftCompare f (toList d1) (toList d2)

instance Ord a => Ord (Deque a) where
    compare = compare1
    {-# INLINE compare #-}

instance Semigroup (Deque a) where
    (<>) = (><)
    {-# INLINE (<>) #-}

instance Monoid (Deque a) where
    mempty = empty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance Functor Deque where
    fmap = map
    {-# INLINE fmap #-}

instance Foldable Deque where
    foldr f acc (Deque _ ls _ _ rs _) = foldr f (foldl (flip f) acc rs) ls

    foldl f acc (Deque _ ls _ _ rs _) = foldr (flip f) (foldl f acc ls) rs

    foldr' f acc (Deque _ ls _ _ rs _) = foldr' f (foldl' (flip f) acc rs) ls

    foldl' f acc (Deque _ ls _ _ rs _) = foldr' (flip f) (foldl' f acc ls) rs

    null (Deque 0 _ _ 0 _ _) = True
    null Deque{} = False
    {-# INLINE null #-}

    length (Deque ll _ _ rl _ _) = ll + rl

instance Traversable Deque where
    traverse f = go
      where
        go d = case viewl d of
            EmptyL -> pure empty
            x :< d' -> (<|) <$> f x <*> go d'
    {-# INLINE traverse #-}

instance IsList (Deque a) where
    type Item (Deque a) = a

    fromList = fromList
    {-# INLINE fromList #-}

    toList = toList
    {-# INLINE toList #-}

instance Applicative Deque where
    pure = singleton
    {-# INLINE pure #-}

    fs <*> xs = foldl' (\acc f -> acc >< map f xs) empty fs

instance Monad Deque where
    xs >>= f = foldl' (\acc x -> acc >< f x) empty xs

instance Alternative Deque where
    empty = empty
    {-# INLINE empty #-}

    (<|>) = (><)
    {-# INLINE (<|>) #-}

instance MonadPlus Deque

instance MonadFail Deque where
    fail _ = empty
    {-# INLINE fail #-}

instance MonadZip Deque where
    mzip = zip
    {-# INLINE mzip #-}

    mzipWith = zipWith
    {-# INLINE mzipWith #-}

    munzip = unzip
    {-# INLINE munzip #-}

instance NFData a => NFData (Deque a) where
    rnf (Deque _ ls _ _ rs _) = rnf ls `seq` rnf rs


-- | /O(1)/.
empty :: Deque a
empty = Deque 0 [] [] 0 [] []
{-# INLINE empty #-}

-- | /O(1)/.
singleton :: a -> Deque a
singleton x = Deque 1 [x] [] 0 [] []

-- | /O(n)/.
fromList :: [a] -> Deque a
fromList = foldr (<|) empty
{-# INLINE fromList #-}

unfoldr :: (b -> Maybe (a, b)) -> b -> Deque a
unfoldr f = go
  where
    go acc = case f acc of
        Nothing -> empty
        Just (x, acc') -> x <| go acc'
{-# INLINE unfoldr #-}

unfoldl :: (b -> Maybe (b, a)) -> b -> Deque a
unfoldl f = go
  where
    go acc = case f acc of
        Nothing -> empty
        Just (acc', x) -> go acc' |> x
{-# INLINE unfoldl #-}

-- | /O(1)/.
(<|) :: a -> Deque a -> Deque a
x <| Deque ll ls ls' rl rs rs' = check $ Deque (ll + 1) (x : ls) (tail ls') rl rs (tail rs')

-- | /O(1)/.
(|>) :: Deque a -> a -> Deque a
Deque ll ls ls' rl rs rs' |> x = check $ Deque ll ls (tail ls') (rl + 1) (x : rs) (tail rs')

data ViewL a = EmptyL | a :< Deque a deriving (Eq, Ord, Show, Read)

-- | /O(1)/.
viewl :: Deque a -> ViewL a
viewl (Deque _ [] _ _ [] _) = EmptyL
viewl (Deque _ [] _ _ (x : _) _) = x :< empty
viewl (Deque ll (x : ls) ls' rl rs rs') = x :< check (Deque (ll - 1) ls (tail (tail ls')) rl rs (tail (tail rs')))

data ViewR a = EmptyR | Deque a :> a deriving (Eq, Ord, Show, Read)

-- | /O(1)/.
viewr :: Deque a -> ViewR a
viewr (Deque _ [] _ _ [] _) = EmptyR
viewr (Deque _ (x : _) _ _ [] _) = empty :> x
viewr (Deque ll ls ls' rl (x : rs) rs') = check (Deque ll ls (tail (tail ls')) (rl - 1) rs (tail (tail rs'))) :> x

-- | /O(1)/.
reverse :: Deque a -> Deque a
reverse (Deque ll ls ls' rl rs rs') = Deque rl rs rs' ll ls ls'

(><) :: Deque a -> Deque a -> Deque a
d1 >< d2
    | length d1 >= length d2 = foldl (|>) d1 d2
    | otherwise = foldr (<|) d2 d1

-- | /O(n)/.
map :: (a -> b) -> Deque a -> Deque b
map f = go
  where
    go d = case viewl d of
        EmptyL -> empty
        x :< xs -> f x <| go xs

filter :: (a -> Bool) -> Deque a -> Deque a
filter f = foldr f' empty
  where
    f' x d
        | f x = x <| d
        | otherwise = d
{-# INLINE filter #-}

partition :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
partition f = foldr f' (empty, empty)
  where
    f' x (d1, d2)
        | f x = (x <| d1, d2)
        | otherwise = (d1, x <| d2)
{-# INLINE partition #-}

mapMaybe :: (a -> Maybe b) -> Deque a -> Deque b
mapMaybe f = foldr f' empty
  where
    f' x d = case f x of
        Just y -> y <| d
        Nothing -> d
{-# INLINE mapMaybe #-}

mapEither :: (a -> Either b c) -> Deque a -> (Deque b, Deque c)
mapEither f = foldr f' (empty, empty)
  where
    f' x (d1, d2) = case f x of
        Left y -> (y <| d1, d2)
        Right y -> (d1, y <| d2)
{-# INLINE mapEither #-}

takeWhileL :: (a -> Bool) -> Deque a -> Deque a
takeWhileL p = go
  where
    go d = case viewl d of
        EmptyL -> empty
        x :< d'
            | p x -> x <| go d'
            | otherwise -> empty
{-# INLINE takeWhileL #-}

takeWhileR :: (a -> Bool) -> Deque a -> Deque a
takeWhileR p = go
  where
    go d = case viewr d of
        EmptyR -> empty
        d' :> x
            | p x -> go d' |> x
            | otherwise -> empty
{-# INLINE takeWhileR #-}

dropWhileL :: (a -> Bool) -> Deque a -> Deque a
dropWhileL p = go
  where
    go d = case viewl d of
        EmptyL -> d
        x :< d'
            | p x -> go d'
            | otherwise -> d
{-# INLINE dropWhileL #-}

dropWhileR :: (a -> Bool) -> Deque a -> Deque a
dropWhileR p = go
  where
    go d = case viewr d of
        EmptyR -> d
        d' :> x
            | p x -> go d'
            | otherwise -> d
{-# INLINE dropWhileR #-}

spanl :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
spanl p = go
  where
    go d = case viewl d of
        EmptyL -> (empty, d)
        x :< d'
            | p x -> let (xs, d'') = go d' in (x <| xs, d'')
            | otherwise -> (empty, d)
{-# INLINE spanl #-}

spanr :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
spanr p = go
  where
    go d = case viewr d of
        EmptyR -> (empty, d)
        d' :> x
            | p x -> let (xs, d'') = go d' in (xs |> x, d'')
            | otherwise -> (empty, d)
{-# INLINE spanr #-}

breakl :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
breakl p = spanl (not . p)
{-# INLINE breakl #-}

breakr :: (a -> Bool) -> Deque a -> (Deque a, Deque a)
breakr p = spanr (not . p)
{-# INLINE breakr #-}

zip :: Deque a -> Deque b -> Deque (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

-- | /O(n)/. A generalized 'zip' zipping with a function.
zipWith :: (a -> b -> c) -> Deque a -> Deque b -> Deque c
zipWith f = go
  where
    go d1 d2 = case (viewl d1, viewl d2) of
        (x :< xs, y :< ys) -> f x y <| go xs ys
        (_, _) -> empty
{-# INLINE zipWith #-}

zip3 :: Deque a -> Deque b -> Deque c -> Deque (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zipWith3 :: (a -> b -> c -> d) -> Deque a -> Deque b -> Deque c -> Deque d
zipWith3 f = go
  where
    go d1 d2 d3 = case (viewl d1, viewl d2, viewl d3) of
        (x :< xs, y :< ys, z :< zs) -> f x y z <| go xs ys zs
        (_, _, _) -> empty
{-# INLINE zipWith3 #-}

unzip :: Deque (a, b) -> (Deque a, Deque b)
unzip = go
  where
    go d = case viewl d of
        EmptyL -> (empty, empty)
        (x, y) :< d' -> let (xs, ys) = unzip d' in (x <| xs, y <| ys)
{-# INLINE unzip #-}

unzip3 :: Deque (a, b, c) -> (Deque a, Deque b, Deque c)
unzip3 = go
  where
    go d = case viewl d of
        EmptyL -> (empty, empty, empty)
        (x, y, z) :< d' -> let (xs, ys, zs) = unzip3 d' in (x <| xs, y <| ys, z <| zs)
{-# INLINE unzip3 #-}
