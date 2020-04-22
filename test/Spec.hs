{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (partition, sort)

import Test.Hspec
import Test.QuickCheck

import Data.AMT (Vector)
import qualified Data.AMT as V
import Data.Deque (Deque)
import qualified Data.Deque as D
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.PrioHeap (PrioHeap)
import qualified Data.PrioHeap as P

instance Arbitrary1 Vector where
   liftArbitrary = fmap V.fromList . liftArbitrary

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = arbitrary1
    {-# INLINE arbitrary #-}

instance Arbitrary1 Deque where
    liftArbitrary = fmap D.fromList . liftArbitrary

instance Arbitrary a => Arbitrary (Deque a) where
    arbitrary = arbitrary1
    {-# INLINE arbitrary #-}

instance (Arbitrary a, Ord a) => Arbitrary (Heap a) where
    arbitrary = fmap H.fromList arbitrary

instance (Arbitrary k, Ord k) => Arbitrary1 (PrioHeap k) where
    liftArbitrary = fmap P.fromList . liftArbitrary . liftArbitrary

instance (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (PrioHeap k a) where
    arbitrary = arbitrary1
    {-# INLINE arbitrary #-}

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs@(_ : _) = Just (init xs, last xs)

main :: IO ()
main = hspec $ do
    describe "Data.AMT" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(v :: Vector Int) -> V.fromList (toList v) === v
        it "satisfies `toList . fromList == id`" $
            property $ \(ls :: [Int]) -> toList (V.fromList ls) === ls
        describe "length" $ do
            it "returns the length" $
                property $ \(v :: Vector Int) -> length v === length (toList v)
            it "returns 0 for the empty vector" $
                length V.empty `shouldBe` 0
        describe "snoc" $ do
            it "appends an element to the back" $
                property $ \(v :: Vector Int) x -> toList (v V.|> x) === toList v ++ [x]
            it "works for the empty vector" $
                property $ \(x :: Int) -> V.empty V.|> x `shouldBe` V.singleton x
        describe "unsnoc" $ do
            it "analyzes the back of the vector" $
                property $ \(v :: Vector Int) -> V.viewr v === fmap (\(xs, x) -> (V.fromList xs, x)) (unsnoc (toList v))
            it "returns Nothing for the empty vector" $
                V.viewr V.empty `shouldBe` (Nothing :: Maybe (Vector Int, Int))
        describe "take" $
            it "takes the first n elements" $
                property $ \n (xs :: [Int]) -> V.take n (V.fromList xs) === V.fromList (take n xs)

    describe "Data.Deque" $ do
            it "satisfies `fromList . toList == id`" $
                property $ \(d :: Deque Int) -> D.fromList (toList d) === d
            it "satisfies `toList . fromList == id`" $
                property $ \(ls :: [Int]) -> toList (D.fromList ls) === ls
            describe "length" $ do
                it "returns the length" $
                    property $ \(d :: Deque Int) -> length d === length (toList d)
                it "returns 0 for the empty deque" $
                    length D.empty `shouldBe` 0
            describe "<|" $ do
                it "appends an element to the front" $
                    property $ \(d :: Deque Int) x -> toList (x D.<| d) === x : toList d
                it "works for the empty deque" $
                    property $ \(x :: Int) -> x D.<| D.empty `shouldBe` D.singleton x
            describe "viewl" $ do
                it "analyzes the front of the deque" $
                    property $ \(d :: Deque Int) -> D.viewl d === fmap (\(x, xs) -> (x, D.fromList xs)) (uncons (toList d))
                it "returns Nothing for the empty deque" $
                    D.viewl D.empty `shouldBe` (Nothing :: Maybe (Int, Deque Int))
            describe "|>" $ do
                it "appends an element to the back" $
                    property $ \(d :: Deque Int) x -> toList (d D.|> x) === toList d ++ [x]
                it "works for the empty deque" $
                    property $ \(x :: Int) -> D.empty D.|> x `shouldBe` D.singleton x
            describe "viewr" $ do
                it "analyzes the back of the deque" $
                    property $ \(d :: Deque Int) -> D.viewr d === fmap (\(xs, x) -> (D.fromList xs, x)) (unsnoc (toList d))
                it "returns Nothing for the empty deque" $
                    D.viewr D.empty `shouldBe` (Nothing :: Maybe (Deque Int, Int))

    describe "Data.Heap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: Heap Int) -> H.fromList (toList h) === h
        describe "size" $ do
            it "returns the size" $
                property $ \(h :: Heap Int) -> H.size h === length (toList h)
            it "returns 0 for the empty heap" $
                H.size H.empty `shouldBe` 0
        describe "union" $
            it "returns the union of two heaps" $
                property $ \(xs :: [Int]) (ys :: [Int]) -> H.fromList xs <> H.fromList ys === H.fromList (xs ++ ys)
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

    describe "Data.PrioHeap" $ do
        it "satisfies `fromList . toList == id`" $
            property $ \(h :: PrioHeap Int ()) -> P.fromList (P.toList h) === h
        describe "size" $ do
            it "returns the size" $
                property $ \(h :: PrioHeap Int Int) -> P.size h === length (toList h)
            it "returns 0 for the empty heap" $
                P.size P.empty `shouldBe` 0
        describe "union" $
            it "returns the union of two heaps" $
                property $ \(xs :: [(Int, ())]) (ys :: [(Int, ())]) -> P.fromList xs <> P.fromList ys === P.fromList (xs ++ ys)
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
