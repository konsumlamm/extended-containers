{-# LANGUAGE ScopedTypeVariables #-}

module Data.Deque.Spec
    ( spec
    ) where

import Data.Foldable (toList)
import Data.List (uncons)

import Test.Hspec
import Test.QuickCheck

import Data.Deque (Deque)
import qualified Data.Deque as D

instance Arbitrary a => Arbitrary (Deque a) where
    arbitrary = fmap D.fromList arbitrary

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

fromViewL :: D.ViewL a -> Maybe (a, Deque a)
fromViewL D.EmptyL = Nothing
fromViewL (x D.:< xs) = Just (x, xs)

fromViewR :: D.ViewR a -> Maybe (Deque a, a)
fromViewR D.EmptyR = Nothing
fromViewR (xs D.:> x) = Just (xs, x)

spec :: Spec
spec = describe "Data.Deque" $ do
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
           property $ \(d :: Deque Int) -> fromViewL (D.viewl d) === fmap (\(x, xs) -> (x, D.fromList xs)) (uncons (toList d))
       it "returns Nothing for the empty deque" $
           D.viewl D.empty `shouldBe` (D.EmptyL :: D.ViewL ())
    describe "|>" $ do
       it "appends an element to the back" $
           property $ \(d :: Deque Int) x -> toList (d D.|> x) === toList d ++ [x]
       it "works for the empty deque" $
           property $ \(x :: Int) -> D.empty D.|> x `shouldBe` D.singleton x
    describe "viewr" $ do
       it "analyzes the back of the deque" $
           property $ \(d :: Deque Int) -> fromViewR (D.viewr d) === fmap (\(xs, x) -> (D.fromList xs, x)) (unsnoc (toList d))
       it "returns Nothing for the empty deque" $
           D.viewr D.empty `shouldBe` (D.EmptyR :: D.ViewR ())
