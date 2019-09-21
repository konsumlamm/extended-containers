-- |
-- = Finite heaps
--
-- The @'Heap' a@ type represents a finite heap (or priority queue) of elements of type @a@.
-- A 'Heap' is strict in its spine. Unlike with sets, duplicate elements are allowed.
--
-- == Performance
--
-- The running time complexities are given, with /n/ referring the the number of elements in the heap.
-- The given running times are worst case.
--
-- == Warning
--
-- The length of a 'Heap' must not exceed @'maxBound' :: 'Int'@.
-- Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the heap is undefined.
--
-- == Implementation
--
-- The implementation of 'Heap' uses leftist heaps.

module Data.Heap
( Heap
-- * Construction
, empty
, singleton
-- ** From Lists
, fromList
, fromAscList
, fromDescList
-- * Insertion/Union
, insert
, union
, unions
-- * Traversal/Filter
, map
, mapMonotonic
, filter
, partition
-- * Query
, member
, size
-- * Min
, lookupMin
, findMin
, deleteMin
, deleteFindMin
, minView
-- * Conversion
-- ** To Lists
, toAscList
, toDescList
) where

import Prelude hiding (filter, map)
import Data.Heap.Internal
