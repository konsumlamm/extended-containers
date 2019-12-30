module Data.PrioHeap.Debug where

import Control.Monad (guard)
import Data.Maybe (isJust)

import Data.PrioHeap.Internal (PrioHeap(..))

valid :: Ord k => PrioHeap k a -> Bool
valid tree = isJust (slowSizeRank tree) && isHeap tree
    where
    slowSizeRank Leaf = Just (0, 0)
    slowSizeRank (Node s r _ _ left right) = do
        (ls, lr) <- slowSizeRank left
        (rs, rr) <- slowSizeRank right
        guard $ s == 1 + ls + rs  -- check if size is valid
            && lr >= rr  -- check if leftist property holds
            && r == 1 + min lr rr  -- check if rank is valid
        pure (s, r)

    isHeap Leaf = True
    isHeap (Node _ _ kx _ left right) = isHeap' kx left && isHeap' kx right
        where
        isHeap' _ Leaf = True
        isHeap' x (Node _ _ ky _ left right) = x <= ky && isHeap' ky left && isHeap' ky right