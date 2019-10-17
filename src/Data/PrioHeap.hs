-- |
-- = Finite priority heaps
--
-- The @'PrioHeap' k a@ type represents a finite heap (or priority queue) from keys/priorities of type @k@ to values of type @a@.
-- A 'PrioHeap' is strict in its spine. Unlike with maps, duplicate keys/priorities are allowed.
--
-- == Performance
--
-- The running time complexities are given, with /n/ referring the the number of elements in the heap.
-- The given running times are worst case.
--
-- == Warning
--
-- The length of a 'PrioHeap' must not exceed @'maxBound' :: 'Int'@.
-- Violation of this condition is not detected and if the length limit is exceeded, the behaviour of the heap is undefined.
--
-- == Implementation
--
-- The implementation of 'PrioHeap' uses leftist heaps.

module Data.PrioHeap
( PrioHeap
-- * Construction
, empty
, singleton
, fromHeap
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
, mapWithKey
, traverseWithKey
, filter
, filterWithKey
, partition
, partitionWithKey
, mapMaybe
, mapMaybeWithKey
, mapEither
, mapEitherWithKey
-- * Folds with key
, foldMapWithKey
, foldlWithKey
, foldrWithKey
, foldlWithKey'
, foldrWithKey'
-- * Query
, member
, size
-- * Min
, adjustMin
, adjustMinWithKey
, lookupMin
, findMin
, deleteMin
, deleteFindMin
, updateMin
, updateMinWithKey
, minView
-- * Conversion
, keysHeap
-- ** To Lists
, smallestN
, values
, keys
, toList
, toAscList
, toDescList
) where

import Prelude hiding (filter, map)
import Data.PrioHeap.Internal
