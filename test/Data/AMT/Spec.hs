{-# LANGUAGE ScopedTypeVariables #-}

module Data.AMT.Spec
    ( spec
    ) where

import Data.Foldable (toList)

import Test.Hspec
import Test.QuickCheck

import Data.AMT (Vector)
import qualified Data.AMT as V

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList arbitrary

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs@(_ : _) = Just (init xs, last xs)

spec :: Spec
spec = describe "Data.AMT" $ do
    it "satisfies `fromList . toList == id`" $
        property $ \(v :: Vector Int) -> V.fromList (toList v) === v
    it "satisfies `toList . fromList == id`" $
        property $ \(ls :: [Int]) -> toList (V.fromList ls) === ls
    describe "length" $ do
        it "returns the length" $
            property $ \(v :: Vector Int) -> length v === length (toList v)
        it "returns 0 for the empty vector" $
            length V.empty `shouldBe` 0
    describe "|>" $ do
        it "appends an element to the back" $
            property $ \(v :: Vector Int) x -> toList (v V.|> x) === toList v ++ [x]
        it "works for the empty vector" $
            property $ \(x :: Int) -> V.empty V.|> x `shouldBe` V.singleton x
    describe "viewr" $ do
        it "analyzes the back of the vector" $
            property $ \(v :: Vector Int) -> V.viewr v === fmap (\(xs, x) -> (V.fromList xs, x)) (unsnoc (toList v))
        it "returns Nothing for the empty vector" $
            V.viewr V.empty `shouldBe` (Nothing :: Maybe (Vector Int, Int))
    describe "take" $
        it "takes the first n elements" $
            property $ \n (xs :: [Int]) -> V.take n (V.fromList xs) === V.fromList (take n xs)
