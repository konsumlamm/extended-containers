module Data.Heap.Debug where

import Control.Monad (guard)
import Data.Maybe (isJust)

import Data.Heap.Internal (Heap(..))

valid :: Ord a => Heap a -> Bool
valid tree = isJust (slowSizeRank tree) && isHeap tree
  where
    slowSizeRank Leaf = Just (0, 0)
    slowSizeRank (Node s r _ left right) = do
        (ls, lr) <- slowSizeRank left
        (rs, rr) <- slowSizeRank right
        guard $ s == 1 + ls + rs  -- check if size is valid
            && lr >= rr  -- check if leftist property holds
            && r == 1 + min lr rr  -- check if rank is valid
        pure (s, r)

    isHeap Leaf = True
    isHeap (Node _ _ x left right) = isHeap' x left && isHeap' x right
      where
        isHeap' _ Leaf = True
        isHeap' x (Node _ _ y left right) = x <= y && isHeap' y left && isHeap' y right