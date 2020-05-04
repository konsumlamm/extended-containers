module Util.Internal.StrictList
    ( List(..)
    , reverse
    ) where

import Prelude hiding (reverse)

import Control.DeepSeq

-- | A strict list.
data List a = Nil | !a `Cons` !(List a)

infixr 5 `Cons`

instance Functor List where
    fmap f = go
      where
        go Nil = Nil
        go (x `Cons` xs) = f x `Cons` go xs
    {-# INLINE fmap #-}

instance Foldable List where
    foldr f acc = go
      where
        go Nil = acc
        go (x `Cons` xs) = f x (go xs)
    {-# INLINE foldr #-}

instance Traversable List where
    traverse f = go
      where
        go Nil = pure Nil
        go (x `Cons` xs) = Cons <$> f x <*> go xs
    {-# INLINE traverse #-}

instance NFData a => NFData (List a) where
    rnf Nil = ()
    rnf (x `Cons` xs) = rnf x `seq` rnf xs

reverse :: List a -> List a
reverse = rev Nil
  where
    rev acc Nil = acc
    rev acc (t `Cons` ts) = rev (t `Cons` acc) ts
{-# INLINE reverse #-}
