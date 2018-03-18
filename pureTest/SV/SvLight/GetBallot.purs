module Test.SV.SvLight.GetBallot where

import Prelude

import Test.SV.Types (SpecType)
import Test.Spec (it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


getBallotTests :: forall e. SpecType e
getBallotTests = do
    it "should remove hanging delegations" do
