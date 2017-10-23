module Test.SV.Main where

import Math
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import Data.Maybe (Maybe(..))
import Node.ChildProcess (CHILD_PROCESS)
import Test.SV.CompleteCycle (completeBallotTest)
import Test.SV.HexBinTests (hexBinTests)
import Test.Spec (describe)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run')


main = run' { slow: 120000, timeout: Just 120000 } [consoleReporter] do
  describe "Voting Auditor Tests" do
    describe "Hex / Binary tests" hexBinTests
  describe "A complete ballot test" completeBallotTest

