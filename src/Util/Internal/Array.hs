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

import Data.List.NonEmpty as L hiding (head, last, take, length)
import Prelude hiding (head, last, take)

import Data.Primitive.SmallArray

type Array a = SmallArray a

empty :: Array a
empty = mempty

singleton :: a -> Array a
singleton x = runSmallArray $ newSmallArray 1 x

snoc :: Array a -> a -> Array a
snoc arr x = runSmallArray $ do
    let size = length arr
    arr' <- newSmallArray (size + 1) x
    copySmallArray arr' 0 arr 0 size
    pure arr'

index :: Int -> Array a -> a
index = flip indexSmallArray

head :: Array a -> a
head arr = indexSmallArray arr 0

last :: Array a -> a
last arr = indexSmallArray arr (length arr - 1)

-- | Update the element at the specified index.
adjust :: Int -> (a -> a) -> Array a -> Array a
adjust i f arr = runSmallArray $ do
    arr' <- thawSmallArray arr 0 (length arr)
    let x = indexSmallArray arr i
    writeSmallArray arr' i (f x)
    pure arr'
{-# INLINE adjust #-}

take :: Int -> Array a -> Array a
take n arr = cloneSmallArray arr 0 n

fromList2 :: a -> a -> Array a
fromList2 x y = runSmallArray $ do
    arr <- newSmallArray 2 x
    writeSmallArray arr 1 y
    pure arr

-- | Convert a full tail into an array.
--
-- > fromTail = A.fromListN tailSize . reverse . toList
fromTail :: Int -> L.NonEmpty a -> Array a
fromTail size (x :| xs) = runSmallArray $ do
    arr <- newSmallArray size x
    let loop _ [] = pure ()
        loop i (y : ys) = writeSmallArray arr i y *> loop (i - 1) ys
    loop (size - 2) xs
    pure arr

-- | Convert an array into a tail.
--
-- > toTail = L.fromList . reverse . toList
toTail :: Array a -> L.NonEmpty a
toTail = L.fromList . foldl (flip (:)) []
