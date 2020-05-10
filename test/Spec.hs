import Test.Hspec (hspec)

import qualified Data.AMT.Spec as AMT
import qualified Data.Deque.Spec as Deque
import qualified Data.Heap.Spec as Heap
import qualified Data.PrioHeap.Spec as PrioHeap

main :: IO ()
main = hspec $ do
    AMT.spec
    Deque.spec
    Heap.spec
    PrioHeap.spec
