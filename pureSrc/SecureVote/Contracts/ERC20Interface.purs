--------------------------------------------------------------------------------
-- | ERC20Interface
--------------------------------------------------------------------------------

module SecureVote.Contracts.ERC20Interface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | CbalanceOfFn
--------------------------------------------------------------------------------


type CbalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

cbalanceOf :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
cbalanceOf x0 cm r = uncurryFields  r $ cbalanceOf' x0 cm
   where
    cbalanceOf' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    cbalanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: CbalanceOfFn)