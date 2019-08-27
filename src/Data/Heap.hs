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