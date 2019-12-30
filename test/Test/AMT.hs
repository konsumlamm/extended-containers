module Test.AMT (testVector) where

import Data.Foldable (toList)

import qualified Data.AMT as V
import Test.Utils (property)

propLength :: [Int] -> Bool
propLength ls = length ls == length (V.fromList ls)

propElem :: Int -> [Int] -> Bool
propElem x xs = elem x (V.fromList xs) == elem x xs

propList :: [Int] -> Bool
propList xs = xs == toList (V.fromList xs)

propTake :: Int -> [Int] -> Bool
propTake n xs = take n xs == toList (V.take n (V.fromList xs))

propSnoc :: [Int] -> Int -> Bool
propSnoc xs x = xs ++ [x] == toList (V.snoc (V.fromList xs) x)

propUnsnoc :: [Int] -> Bool
propUnsnoc xs = unsnoc xs == fmap (\(v, x) -> (toList v, x)) (V.unsnoc (V.fromList xs))
  where
    unsnoc [] = Nothing
    unsnoc ls = Just (init ls, last ls)

testVector :: IO ()
testVector = do
    property "length" propLength
    property "elem" propElem
    property "toList . fromList = id" propList
    property "take" propTake
    property "snoc" propSnoc
    property "unsnoc" propUnsnoc