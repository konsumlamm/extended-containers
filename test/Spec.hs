import Test.AMT (testVector)
import Test.Heap (testHeap)
import Test.PrioHeap (testPrioHeap)

main :: IO ()
main = do
    putStrLn "\ntesting Data.AMT:"
    testVector
    putStrLn "\ntesting Data.Heap:"
    testHeap
    putStrLn "\ntesting Data.PrioHeap:"
    testPrioHeap