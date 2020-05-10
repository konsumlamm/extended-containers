{-# LANGUAGE ScopedTypeVariables #-}

module Data.Heap.Spec
    ( spec
    ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (partition, sort, uncons)

import Test.Hspec
import Test.QuickCheck

import Data.Heap (Heap)
import qualified Data.Heap as H

instance (Arbitrary a, Ord a) => Arbitrary (Heap a) where
    arbitrary = fmap H.fromList arbitrary

spec :: Spec
spec = describe "Data.Heap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: Heap Int) -> H.fromList (toList h) === h
        describe "size" $ do
            it "returns the size" $
                property $ \(h :: Heap Int) -> H.size h === length (toList h)
            it "returns 0 for the empty heap" $
                H.size H.empty `shouldBe` 0
        describe "union" $
            it "returns the union of two heaps" $
                property $ \(xs :: [Int]) (ys :: [Int]) -> H.fromList xs `H.union` H.fromList ys === H.fromList (xs ++ ys)
        describe "insert" $
            it "inserts an element" $
                property $ \(xs :: [Int]) (x :: Int) -> H.insert x (H.fromList xs) === H.fromList (x : xs)
        describe "deleteMin" $
            it "deletes the minimum element" $
                property $ \(xs :: [Int]) -> H.deleteMin (H.fromList xs) === maybe H.empty (H.fromList . snd) (uncons (sort xs))
        describe "filter" $
            it "filters the elements that satisfy the predicate" $
                property $ \(xs :: [Int]) -> H.filter even (H.fromList xs) === H.fromList (filter even xs)
        describe "partition" $
            it "partitions the elements based on the predicate" $
                property $ \(xs :: [Int]) -> H.partition even (H.fromList xs) === bimap H.fromList H.fromList (partition even xs)
        describe "heapsort" $
            it "sorts a list" $
                property $ \(ls :: [Int]) -> H.heapsort ls === sort ls
