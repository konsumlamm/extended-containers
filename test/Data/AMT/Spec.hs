module Data.AMT.Spec
    ( spec
    ) where

import Data.Foldable (toList)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.AMT as V

default (Int)

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc ls = Just (init ls, last ls)

(!?) :: [a] -> Int -> Maybe a
ls !? i
    | i < 0 || i >= length ls = Nothing
    | otherwise = Just (ls !! i)

spec :: Spec
spec = describe "Data.AMT" $ do
    prop "satisfies `fromList . toList == id`" $ \v -> V.fromList (toList v) === v
    prop "satisfies `toList . fromList == id`" $ \ls -> toList (V.fromList ls) === ls

    describe "length" $ do
        prop "returns the length" $ \ls -> length (V.fromList ls) === length ls
        it "returns 0 for the empty vector" $ length V.empty `shouldBe` 0

    describe "|>" $ do
        prop "appends an element to the back" $ \v x -> toList (v V.|> x) === toList v ++ [x]
        prop "works for the empty vector" $ \x -> V.empty V.|> x `shouldBe` V.singleton x

    describe "viewr" $ do
        prop "analyzes the back of the vector" $ \v -> V.viewr v === fmap (\(xs, x) -> (V.fromList xs, x)) (unsnoc (toList v))
        it "returns Nothing for the empty vector" $ V.viewr V.empty `shouldBe` Nothing

    describe "take" $ do
        prop "takes the first n elements" $ \(Positive n) xs -> V.take n (V.fromList xs) === V.fromList (take n xs)
        prop "returns the empty vector for non-positive n" $ \(NonPositive n) v -> V.take n v === V.empty
        prop "does nothing for the empty vector" $ \n -> V.take n V.empty === V.empty

    describe "lookup" $ do
        prop "returns the ith element" $ \(NonNegative i) v -> V.lookup i v === toList v !? i
        prop "returns Nothing for negative indices" $ \(Negative i) v -> V.lookup i v === Nothing
        prop "returns Nothing for the empty vector" $ \i -> V.lookup i V.empty === Nothing
