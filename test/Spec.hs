import Test.Hspec (hspec)

import qualified Data.AMT.Spec as AMT
import qualified Data.Heap.Spec as Heap
import qualified Data.PrioHeap.Spec as PrioHeap

main :: IO ()
main = hspec $ do
    AMT.spec
    Heap.spec
    PrioHeap.spec
