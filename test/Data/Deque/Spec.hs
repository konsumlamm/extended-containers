module Data.Deque.Spec
    ( spec
    ) where

import Data.Foldable (toList)
import Data.List (uncons)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import qualified Data.Deque as D

default (Int)

instance Arbitrary a => Arbitrary (D.Deque a) where
    arbitrary = fmap D.fromList arbitrary

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

fromViewL :: D.ViewL a -> Maybe (a, D.Deque a)
fromViewL D.EmptyL = Nothing
fromViewL (x D.:< xs) = Just (x, xs)

fromViewR :: D.ViewR a -> Maybe (D.Deque a, a)
fromViewR D.EmptyR = Nothing
fromViewR (xs D.:> x) = Just (xs, x)

spec :: Spec
spec = describe "Data.Deque" $ do
    prop "satisfies `fromList . toList == id`" $ \d -> D.fromList (toList d) === d
    prop "satisfies `toList . fromList == id`" $ \ls -> toList (D.fromList ls) === ls

    describe "length" $ do
       prop "returns the length" $ \ls -> length (D.fromList ls) === length ls
       it "returns 0 for the empty deque" $ length D.empty `shouldBe` 0

    describe "<|" $ do
       prop "appends an element to the front" $ \d x -> toList (x D.<| d) === x : toList d
       prop "works for the empty deque" $ \x -> x D.<| D.empty `shouldBe` D.singleton x

    describe "viewl" $ do
       prop "analyzes the front of the deque" $ \d -> fromViewL (D.viewl d) === fmap (\(x, xs) -> (x, D.fromList xs)) (uncons (toList d))
       it "returns Nothing for the empty deque" $ D.viewl D.empty `shouldBe` D.EmptyL

    describe "|>" $ do
       prop "appends an element to the back" $ \d x -> toList (d D.|> x) === toList d ++ [x]
       prop "works for the empty deque" $ \x -> D.empty D.|> x `shouldBe` D.singleton x

    describe "viewr" $ do
       prop "analyzes the back of the deque" $ \d -> fromViewR (D.viewr d) === fmap (\(xs, x) -> (D.fromList xs, x)) (unsnoc (toList d))
       it "returns Nothing for the empty deque" $ D.viewr D.empty `shouldBe` D.EmptyR

    describe "reverse" $ do
        prop "satisfies `reverse . reverse == id`" $ \d -> D.reverse (D.reverse d) === d
        prop "reverses the deque" $ \d -> toList (D.reverse d) === reverse (toList d)
        it "works for the empty deque" $ D.reverse D.empty `shouldBe` D.empty

    describe "><" $ do
        prop "concatenates the deques" $ \d1 d2 -> toList (d1 D.>< d2) === toList d1 ++ toList d2
        prop "does nothing for the empty deque" $ \d -> d D.>< D.empty === d .&&. D.empty D.>< d === d
