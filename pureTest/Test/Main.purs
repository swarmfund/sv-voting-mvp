module Test.Main where
  
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import Node.ChildProcess (CHILD_PROCESS)
import Prelude (Unit)
import Test.SV.Main as SV
import Test.Spec.QuickCheck (QCRunnerEffects)

main = SV.main