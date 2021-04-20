-- | The functions in this module perform no bounds checking.

module Util.Internal.Array
    ( Array
    , empty
    , singleton
    , snoc
    , index
    , head, last
    , adjust
    , take
    , fromList2
    , fromTail
    , toTail
    ) where

-- TODO: use SmallArray or similar

import Data.List.NonEmpty as L hiding (head, last, take)
import Prelude hiding (head, last, take)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

type Array a = V.Vector a

empty :: Array a
empty = V.empty

singleton :: a -> Array a
singleton = V.singleton

snoc :: Array a -> a -> Array a
snoc = V.snoc

index :: Int -> Array a -> a
index = flip V.unsafeIndex

head :: Array a -> a
head = V.unsafeHead

last :: Array a -> a
last = V.unsafeLast

-- | Update the element at the specified index.
adjust :: Int -> (a -> a) -> Array a -> Array a
adjust i f = V.modify (\v -> M.unsafeModify v f i)
{-# INLINE adjust #-}

take :: Int -> Array a -> Array a
take = V.unsafeTake

fromList2 :: a -> a -> Array a
fromList2 x y = V.fromListN 2 [x, y]

-- | Convert a full tail into an array.
--
-- > fromTail = A.fromListN tailSize . reverse . toList
fromTail :: Int -> L.NonEmpty a -> Array a
fromTail size (x :| xs) = V.create $ do
    v <- M.new size
    M.unsafeWrite v (size - 1) x  -- mask = tailSize - 1
    let loop _ [] = pure ()
        loop i (y : ys) = M.unsafeWrite v i y *> loop (i - 1) ys
    loop (size - 2) xs
    pure v

-- | Convert an array into a tail.
--
-- > toTail = L.fromList . reverse . toList
toTail :: Array a -> L.NonEmpty a
toTail = L.fromList . foldl (flip (:)) []
