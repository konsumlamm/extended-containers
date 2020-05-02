{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeFamilies #-}
#endif

{- TODO:
    zip, zipWith, zip3, zipWith3, unzip, unzip3?
    fromFunction, replicate, replicateA, iterateN?
    take, drop, splitAt
    takeWhileL, takeWhileR, dropWhileL, dropWhileR, spanl, spanr, breakl, breakr
    partition, filter, append/concat?
    see https://hackage.haskell.org/package/containers/docs/Data-Sequence.html
-}
-- TODO: pattern synonyms

{- |
= Finite deques

The @'Deque' a@ type represents a finite deque (double ended queue) of elements of type @a@.

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

module Data.Deque
    ( Deque
    , empty
    , singleton
    , fromList
    , (<|)
    , (|>)
    , viewl
    , viewr
    , reverse
    , map
    ) where

import Data.Foldable (toList)
import Data.Functor.Classes
import qualified Data.List as List
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts
#endif
import Prelude hiding (map, reverse, tail)
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec)

infixr 5 <|
infixl 5 |>

tail :: [a] -> [a]
tail [] = []
tail (_ : xs) = xs

-- | Invariant: @length ls <= 3 * length rs + 1 && length rs <= 3 * length ls + 1@.
data Deque a = Deque !Int [a] [a] !Int [a] [a]

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
#ifdef __GLASGOW_HASKELL__
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        pure (fromList xs)
#else
    readsPrec = readsPrec1
    {-# INLINE readsPrec #-}
#endif

instance Functor Deque where
    fmap = map
    {-# INLINE fmap #-}

instance Foldable Deque where
    foldr f acc (Deque _ ls _ _ rs _) = foldr f (foldl (flip f) acc rs) ls

    length (Deque ll _ _ rl _ _) = ll + rl

instance Traversable Deque where
    traverse f = go
      where
        go d = case viewl d of
            Nothing -> pure empty
            Just (x, d') -> (<|) <$> f x <*> go d'
    {-# INLINE traverse #-}

{-
instance Eq a => Eq (Deque a) where
    (==) = eq1
    {-# INLINE (==) #-}
-}

#ifdef __GLASGOW_HASKELL__
instance IsList (Deque a) where
    type Item (Deque a) = a

    fromList = fromList
    {-# INLINE fromList #-}

    toList = toList
    {-# INLINE toList #-}
#endif

-- TODO: instances
-- Show1, Show, Read1, Read, Eq1, Ord1, Ord
-- Semigroup, Monoid
-- Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadZip


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

-- TODO: unfoldr :: (b -> Maybe (a, b)) -> b -> Deque a

-- TODO: unfoldl :: (b -> Maybe (b, a)) -> b -> Deque a

-- | /O(1)/.
(<|) :: a -> Deque a -> Deque a
x <| Deque ll ls ls' rl rs rs' = check $ Deque (ll + 1) (x : ls) (tail ls') rl rs (tail rs')

-- | /O(1)/.
viewl :: Deque a -> Maybe (a, Deque a)
viewl (Deque _ [] _ _ [] _) = Nothing
viewl (Deque _ [] _ _ (x : _) _) = Just (x, empty)
viewl (Deque ll (x : ls) ls' rl rs rs') = Just (x, check $ Deque (ll - 1) ls (tail (tail ls')) rl rs (tail (tail rs')))

-- | /O(1)/.
(|>) :: Deque a -> a -> Deque a
Deque ll ls ls' rl rs rs' |> x = check $ Deque ll ls (tail ls') (rl + 1) (x : rs) (tail rs')

-- | /O(1)/.
viewr :: Deque a -> Maybe (Deque a, a)
viewr (Deque _ [] _ _ [] _) = Nothing
viewr (Deque _ (x : _) _ _ [] _) = Just (empty, x)
viewr (Deque ll ls ls' rl (x : rs) rs') = Just (check $ Deque ll ls (tail (tail ls')) (rl - 1) rs (tail (tail rs')), x)

-- | /O(1)/.
reverse :: Deque a -> Deque a
reverse (Deque ll ls ls' rl rs rs') = Deque rl rs rs' ll ls ls'

-- | /O(n)/.
-- TODO
map :: (a -> b) -> Deque a -> Deque b
map f (Deque ll ls ls' rl rs rs') = Deque ll (fmap f ls) (fmap f ls') rl (fmap f rs) (fmap f rs')
