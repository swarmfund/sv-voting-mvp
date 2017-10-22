module SecureVote.Web3.TestRPC where
  

import Control.Monad.Aff (Aff, Error)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn1, runFn1)
import Prelude ((<<<))


data TestRPCServer = TestRPCServer TestRPCServer
  

foreign import testRpcImpl :: forall e. Number -> (EffFnAff (| e) TestRPCServer)


testRpcServer :: forall e. Number -> Aff (| e) TestRPCServer
testRpcServer = fromEffFnAff <<< testRpcImpl