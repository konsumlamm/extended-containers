{-# LANGUAGE CPP #-}

module Data.List.Strict
( List(..)
, (++)
, head
, last
, tail
, init
, uncons
, map
, reverse
, intersperse
, intercalate
, concat
, concatMap
-- * Zipping
, zip
, zip3
, zipWith
, zipWith3
, fromList
) where

import Prelude hiding ((++), head, last, tail, init, map, reverse, concat, concatMap, takeWhile, dropWhile, lookup, filter, zip, zip3, zipWith, zipWith3)
import Data.Foldable (toList)

infixr 5 :!
infixr 5 ++

data List a = Nil | a :! !(List a)

errorEmpty :: String -> a
errorEmpty s = error $ "List." <> s <> ": empty list"

instance Semigroup (List a) where
    (<>) = (++)

instance Monoid (List a) where
    mempty = Nil

instance Foldable List where
    foldr _ a Nil = a
    foldr f a (x :! xs) = f x (foldr f a xs)

    foldl _ a Nil = a
    foldl f a (x :! xs) = foldl f (f a x) xs

    null Nil = True
    null (_ :! _) = False

instance Functor List where
    fmap = map

instance Applicative List where
    pure x = x :! Nil

    fs <*> xs = foldr (\f a -> map f xs ++ a) Nil fs

instance Monad List where
    xs >>= f = concatMap f xs

    fail _ = Nil
{-
#ifdef __GLASGOW_HASKELL__
instance IsList (List a) where
    type Item (List a) = a
    fromList = fromList
    toList = toList
#endif
-}

(++) :: List a -> List a -> List a
Nil ++ ys = ys
(x :! xs) ++ ys = x :! (xs ++ ys)

-- | /O(1)/.
head :: List a -> a
head Nil = errorEmpty "head"
head (x :! _) = x

last :: List a -> a
last Nil = errorEmpty "last"
last (x :! Nil) = x
last (x :! xs) = last xs

-- | /O(1)/.
tail :: List a -> List a
tail Nil = errorEmpty "tail"
tail (_ :! xs) = xs

init :: List a -> List a
init Nil = errorEmpty "init"
init (_ :! Nil) = Nil
init (x :! xs) = x :! init xs

-- | /O(1)/.
uncons :: List a -> Maybe (a, List a)
uncons Nil = Nothing
uncons (x :! xs) = Just (x, xs)


-- | /O(n)/.
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (x :! xs) = f x :! map f xs

reverse :: List a -> List a
reverse Nil = Nil
reverse (x :! xs) = reverse xs ++ (x :! Nil)

intersperse :: a -> List a -> List a
intersperse _ Nil = Nil
intersperse _ (x :! Nil) = x :! Nil
intersperse sep (x :! xs) = x :! sep :! intersperse sep xs

intercalate :: List a -> List (List a) -> List a
intercalate xs xss = concat (intersperse xs xss)


concat :: Foldable t => t (List a) -> List a
concat = foldr (++) Nil

concatMap :: Foldable t => (a -> List b) -> t a -> List b
concatMap f = foldr (\x a -> f x ++ a) Nil


takeWhile :: (a -> Bool) -> List a -> List a
takewhile _ Nil = Nil
takeWhile f (x :! xs) = if f x then x :! takeWhile f xs else Nil

dropWhile :: (a -> Bool) -> List a -> List a
dropwhile _ Nil = Nil
dropWhile f ls@(x :! xs) = if f x then dropWhile f xs else ls


lookup :: Eq a => a -> List (a, b) -> Maybe b
lookup _ Nil = Nothing
lookup x ((a, b) :! xs) = if x == a then Just b else lookup x xs

filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter f (x :! xs) = if f x then x :! filter f xs else filter f xs


zip :: List a -> List b -> List (a, b)
zip = zipWith (,)

zip3 :: List a -> List b -> List c -> List (a, b, c)
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith z (a :! as) (b :! bs) = z a b :! zipWith z as bs
zipWith _ _ _ = Nil

zipWith3 :: (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 z (a :! as) (b :! bs) (c :! cs) = z a b c :! zipWith3 z as bs cs
zipWith3 _ _ _ _ = Nil


-- | /O(n)/.
fromList :: [a] -> List a
fromList [] = Nil
fromList (x : xs) = x :! fromList xs