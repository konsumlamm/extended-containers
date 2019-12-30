module Test.Utils (property) where

import Test.QuickCheck (Testable, quickCheck)

property :: Testable p => String -> p -> IO ()
property msg prop = putStrLn msg *> quickCheck prop