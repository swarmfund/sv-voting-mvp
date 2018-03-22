module Test.SV.UtilTests where


import Prelude

import SecureVote.Utils.Array (chunk)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)


utilTests :: forall e. SpecType e
utilTests = do
    it "should chunk arrays correctly" do
        ([] :: Array (Array Int)) `shouldEqual` chunk 5 ([] :: Array Int)
        [[0,1,2,3,4],[5]] `shouldEqual` chunk 5 [0,1,2,3,4,5]
        [[0,1],[2,3],[4,5]] `shouldEqual` chunk 2 [0,1,2,3,4,5]
        [[0,1,2,3],[4,5]] `shouldEqual` chunk 4 [0,1,2,3,4,5]
        [[0],[1]] `shouldEqual` chunk 1 [0,1]
        [[0,1],[2,3],[4]] `shouldEqual` chunk 2 [0,1,2,3,4]
