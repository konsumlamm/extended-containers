{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (toList)
import Data.List (sort)

import Test.Hspec
import Test.QuickCheck

import Data.AMT (Vector)
import qualified Data.AMT as V
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.PrioHeap (PrioHeap)
import qualified Data.PrioHeap as P

instance Arbitrary1 Vector where
   liftArbitrary = fmap V.fromList . liftArbitrary

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = arbitrary1

instance (Arbitrary a, Ord a) => Arbitrary (Heap a) where
    arbitrary = fmap H.fromList arbitrary

instance (Arbitrary k, Ord k) => Arbitrary1 (PrioHeap k) where
    liftArbitrary = fmap P.fromList . liftArbitrary . liftArbitrary

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (PrioHeap k a) where
    arbitrary = arbitrary1

{- Test.AMT
propElem :: Int -> [Int] -> Bool
propElem x xs = elem x (V.fromList xs) == elem x xs

propTake :: Int -> [Int] -> Bool
propTake n xs = take n xs == toList (V.take n (V.fromList xs))

propUnsnoc :: [Int] -> Bool
propUnsnoc xs = unsnoc xs == fmap (\(v, x) -> (toList v, x)) (V.unsnoc (V.fromList xs))
  where
    unsnoc [] = Nothing
    unsnoc ls = Just (init ls, last ls)
-}
{- Test.Heap
propFilter :: [Int] -> Bool
propFilter ls = H.filter even (H.fromList ls) == H.fromList (filter even ls)

propPartition :: [Int] -> Bool
propPartition ls = H.partition even (H.fromList ls) == both H.fromList (partition even ls)
  where
    both f (x, y) = (f x, f y)
-}

main :: IO ()
main = hspec $ do
    describe "Data.AMT" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(v :: Vector Int) -> V.fromList (toList v) === v
        it "satisfies `toList . fromList == id`" $
            property $ \(ls :: [Int]) -> toList (V.fromList ls) === ls
        describe "length" $ do
            it "does the same as using list" $
                property $ \(v :: Vector Int) -> length v === length (toList v)
            it "returns 0 for the empty vector" $
                length V.empty `shouldBe` 0
        describe "snoc" $ do
            it "does the same as using list" $
                property $ \(v :: Vector Int) x -> toList (V.snoc v x) === toList v ++ [x]
            it "works for the empty vector" $
                property $ \(x :: Int) -> V.snoc V.empty x `shouldBe` V.singleton x

    describe "Data.Heap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: Heap Int) -> H.fromList (toList h) === h
        describe "size" $ do
            it "does the same as using list" $
                property $ \(v :: Heap Int) -> H.size v === length (toList v)
            it "returns 0 for the empty vector" $
                H.size H.empty `shouldBe` 0
        describe "union" $
            it "does the same as using list" $
                property $ \(xs :: [Int]) (ys :: [Int]) -> H.fromList xs <> H.fromList ys === H.fromList (xs ++ ys)
        describe "heapsort" $
            it "sorts a list" $
                property $ \(ls :: [Int]) -> H.heapsort ls === sort ls

    describe "Data.PrioHeap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: PrioHeap Int ()) -> P.fromList (P.toList h) === h
        describe "size" $ do
            it "does the same as using list" $
                property $ \(v :: Heap Int) -> H.size v === length (toList v)
            it "returns 0 for the empty vector" $
                H.size H.empty `shouldBe` 0
        describe "union" $
            it "does the same as using list" $
                property $ \(xs :: [Int]) (ys :: [Int]) -> H.fromList xs <> H.fromList ys === H.fromList (xs ++ ys)
