module Data.PrioHeap.Spec
    ( spec
    ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (partition, sort, uncons)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.PrioHeap as P

default (Int)

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (P.PrioHeap k a) where
    arbitrary = fmap P.fromList arbitrary

fromList :: [(Int, ())] -> P.PrioHeap Int ()
fromList = P.fromList

spec :: Spec
spec = describe "Data.PrioHeap" $ do
        prop "satisfies `fromList . toList == id`" $ \h -> fromList (P.toList h) === h

        describe "size" $ do
            prop "returns the size" $ \h -> P.size h === length (toList h)
            it "returns 0 for the empty heap" $ P.size P.empty `shouldBe` 0

        describe "union" $ do
            prop "returns the union of two heaps" $ \xs ys -> P.union (fromList xs) (fromList ys) === fromList (xs ++ ys)
            prop "empty heap is neutral element" $ \h -> P.union h P.empty === h .&&. P.union P.empty h === h

        describe "insert" $ do
            prop "inserts an element" $ \xs x -> P.insert x () (fromList xs) === fromList ((x, ()) : xs)
            prop "works for the empty heap" $ \k v -> P.insert k v P.empty === P.singleton k v

        describe "deleteMin" $ do
            prop "deletes the minimum element" $ \xs -> P.deleteMin (fromList xs) === maybe P.empty (fromList . snd) (uncons (sort xs))
            prop "works for the empty heap" $ P.deleteMin P.empty `shouldBe` P.empty

        describe "filterWithKey" $ do
            prop "filters the elements that satisfy the predicate" $ \xs -> P.filterWithKey (\k () -> even k) (fromList xs) === fromList (filter (even . fst) xs)

        describe "partitionWithKey" $ do
            prop "partitions the elements based on the predicate" $ \xs -> P.partitionWithKey (\k () -> even k) (fromList xs) === bimap fromList fromList (partition (even . fst) xs)
