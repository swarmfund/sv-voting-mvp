module Test.SV.Types where

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Crypt.NaCl (NACL_RANDOM)
import IPFS (IPFSEff)
import Network.Ethereum.Web3 (ETH)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Prelude (Unit)
import Test.Spec (Spec)

type SpecType e = Spec ( random :: RANDOM
                       , cp :: CHILD_PROCESS
                       , console :: CONSOLE
                       , ref :: REF
                       , now :: NOW
                       , naclRandom :: NACL_RANDOM
                       , buffer :: BUFFER
                       , eth :: ETH
                       , avar :: AVAR
                       , ipfs :: IPFSEff
                       | e ) Unit
