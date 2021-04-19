module Data.Heap.Spec
    ( spec
    ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (partition, sort, uncons)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Heap as H

default (Int)

instance (Arbitrary a, Ord a) => Arbitrary (H.Heap a) where
    arbitrary = fmap H.fromList arbitrary

spec :: Spec
spec = describe "Data.Heap" $ do
        prop "satisfies `fromList . toList == id`" $ \h -> H.fromList (toList h) === h

        describe "size" $ do
            prop "returns the size" $ \h -> H.size h === length (toList h)
            it "returns 0 for the empty heap" $ H.size H.empty `shouldBe` 0

        describe "union" $ do
            prop "returns the union of two heaps" $ \xs ys -> H.union (H.fromList xs) (H.fromList ys) === H.fromList (xs ++ ys)
            prop "empty heap is neutral element" $ \h -> H.union h H.empty === h .&&. H.union H.empty h === h

        describe "insert" $ do
            prop "inserts an element" $ \xs x -> H.insert x (H.fromList xs) === H.fromList (x : xs)
            prop "works for the empty heap" $ \x -> H.insert x H.empty === H.singleton x

        describe "deleteMin" $ do
            prop "deletes the minimum element" $ \xs -> H.deleteMin (H.fromList xs) === maybe H.empty (H.fromList . snd) (uncons (sort xs))
            prop "works for the empty heap" $ H.deleteMin H.empty `shouldBe` H.empty

        describe "filter" $ do
            prop "filters the elements that satisfy the predicate" $ \xs -> H.filter even (H.fromList xs) === H.fromList (filter even xs)

        describe "partition" $ do
            prop "partitions the elements based on the predicate" $ \xs -> H.partition even (H.fromList xs) === bimap H.fromList H.fromList (partition even xs)

        describe "heapsort" $ do
            prop "sorts a list" $ \ls -> H.heapsort ls === sort ls
