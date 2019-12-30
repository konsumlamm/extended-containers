module Test.PrioHeap (testPrioHeap) where

import qualified Data.PrioHeap as H
import Data.PrioHeap.Debug (valid)
import Test.Utils (property)

propValid :: [(Int, Int)] -> Bool
propValid ls = valid (H.fromList ls)

propSize :: [(Int, Int)] -> Bool
propSize ls = length ls == H.size (H.fromList ls)

propMember :: Int -> [(Int, Int)] -> Bool
propMember x xs = H.member x (H.fromList xs) == elem x (map fst xs)

testPrioHeap :: IO ()
testPrioHeap = do
    property "valid" propValid
    property "size" propSize
    property "member" propMember