{- |
= Finite heaps

The @'Heap' a@ type represents a finite heap (or priority queue) of elements of type @a@.
A 'Heap' is strict in its spine. Unlike with sets, duplicate elements are allowed.

== Performance

The running time complexities are given, with /n/ referring the the number of elements in the heap.
The given running times are worst case.

== Warning

The length of a 'Heap' must not exceed @'maxBound' :: 'Int'@.
Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the heap is undefined.

== Implementation

The implementation uses skew binomial heaps, as described in

* Chris Okasaki, \"Purely Functional Data Structures\", 1998
-}

module Data.Heap
    ( Heap
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
    -- * Conversion
    -- ** To Lists
    , toAscList, toDescList
    -- * Heapsort
    , heapsort
    ) where

import Prelude hiding (break, drop, dropWhile, filter, map, span, splitAt, take, takeWhile)

import Data.Heap.Internal
