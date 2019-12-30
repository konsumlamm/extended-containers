module Test.Heap (testHeap) where

import Data.List (partition, sort)

import qualified Data.Heap as H
import Data.Heap.Debug (valid)
import Test.Utils (property)

propValid :: [Int] -> Bool
propValid ls = valid (H.fromList ls)

propSize :: [Int] -> Bool
propSize ls = length ls == H.size (H.fromList ls)

propMember :: Int -> [Int] -> Bool
propMember x xs = H.member x (H.fromList xs) == elem x xs

propSort :: [Int] -> Bool
propSort ls = sort ls == H.heapsort ls

propSmallestN :: Int -> [Int] -> Bool
propSmallestN n ls = take n (sort ls) == fst (H.smallestN n (H.fromList ls))

propUnion :: [Int] -> [Int] -> Bool
propUnion as bs = H.union (H.fromList as) (H.fromList bs) == H.fromList (as ++ bs)

propFilter :: [Int] -> Bool
propFilter ls = H.filter even (H.fromList ls) == H.fromList (filter even ls)

propPartition :: [Int] -> Bool
propPartition ls = H.partition even (H.fromList ls) == both H.fromList (partition even ls)
  where  
    both f (x, y) = (f x, f y)

testHeap :: IO ()
testHeap = do
    property "valid" propValid
    property "size" propSize
    property "member" propMember
    property "heapsort" propSort
    property "smallestN" propSmallestN
    property "union" propUnion
    property "filter" propFilter
    property "partition" propPartition