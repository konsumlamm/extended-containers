{-# LANGUAGE ScopedTypeVariables #-}

module Data.PrioHeap.Spec
    ( spec
    ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (partition, sort, uncons)

import Test.Hspec
import Test.QuickCheck

import Data.PrioHeap (PrioHeap)
import qualified Data.PrioHeap as P

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (PrioHeap k a) where
    arbitrary = fmap P.fromList arbitrary

spec :: Spec
spec = describe "Data.PrioHeap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: PrioHeap Int ()) -> P.fromList (P.toList h) === h
        describe "size" $ do
            it "returns the size" $
                property $ \(h :: PrioHeap Int Int) -> P.size h === length (toList h)
            it "returns 0 for the empty heap" $
                P.size P.empty `shouldBe` 0
        describe "union" $
            it "returns the union of two heaps" $
                property $ \(xs :: [(Int, ())]) (ys :: [(Int, ())]) -> P.fromList xs `P.union` P.fromList ys === P.fromList (xs ++ ys)
        describe "insert" $
            it "inserts an element" $
                property $ \(xs :: [(Int, ())]) (x :: Int) -> P.insert x () (P.fromList xs) === P.fromList ((x, ()) : xs)
        describe "deleteMin" $
            it "deletes the minimum element" $
                property $ \(xs :: [(Int, ())]) -> P.deleteMin (P.fromList xs) === maybe P.empty (P.fromList . snd) (uncons (sort xs))
        describe "filterWithKey" $
            it "filters the elements that satisfy the predicate" $
                property $ \(xs :: [(Int, ())]) -> P.filterWithKey (const . even) (P.fromList xs) === P.fromList (filter (even . fst) xs)
        describe "partitionWithKey" $
            it "partitions the elements based on the predicate" $
                property $ \(xs :: [(Int, ())]) -> P.partitionWithKey (const . even) (P.fromList xs) === bimap P.fromList P.fromList (partition (even . fst) xs)
