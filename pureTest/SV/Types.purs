module Test.SV.Types where

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Crypt.NaCl (NACL_RANDOM)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.Process (PROCESS)
import Node.Process as NP
import Prelude (Unit)
import Test.Spec (Spec)

type SpecType e = Spec ( random :: RANDOM, cp :: CHILD_PROCESS, console :: CONSOLE, avar :: AVAR, now :: NOW, naclRandom :: NACL_RANDOM, buffer :: BUFFER | e ) Unit

