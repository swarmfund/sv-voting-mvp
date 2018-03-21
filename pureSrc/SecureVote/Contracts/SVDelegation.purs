--------------------------------------------------------------------------------
-- | SVDelegation
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVDelegation where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple4, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | CtotalDelegationsFn
--------------------------------------------------------------------------------


type CtotalDelegationsFn = Tagged (SProxy "totalDelegations()") (Tuple0 )

ctotalDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
ctotalDelegations x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CtotalDelegationsFn)

--------------------------------------------------------------------------------
-- | ChistoricalDelegationsFn
--------------------------------------------------------------------------------


type ChistoricalDelegationsFn = Tagged (SProxy "historicalDelegations(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

chistoricalDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
chistoricalDelegations x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: ChistoricalDelegationsFn)

--------------------------------------------------------------------------------
-- | C_rawGetTokenDelegationFn
--------------------------------------------------------------------------------


type C_rawGetTokenDelegationFn = Tagged (SProxy "_rawGetTokenDelegation(address,address)") (Tuple2 Address Address)

c_rawGetTokenDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { _voter :: Address, _tokenContract :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
c_rawGetTokenDelegation x0 cm r = uncurryFields  r $ c_rawGetTokenDelegation' x0 cm
   where
    c_rawGetTokenDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_voter") Address -> Tagged (SProxy "_tokenContract") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
    c_rawGetTokenDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: C_rawGetTokenDelegationFn)

--------------------------------------------------------------------------------
-- | CresolveDelegationFn
--------------------------------------------------------------------------------


type CresolveDelegationFn = Tagged (SProxy "resolveDelegation(address,address)") (Tuple2 Address Address)

cresolveDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: Address, tokenContract :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
cresolveDelegation x0 cm r = uncurryFields  r $ cresolveDelegation' x0 cm
   where
    cresolveDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") Address -> Tagged (SProxy "tokenContract") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
    cresolveDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: CresolveDelegationFn)

--------------------------------------------------------------------------------
-- | CownerFn
--------------------------------------------------------------------------------


type CownerFn = Tagged (SProxy "owner()") (Tuple0 )

cowner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
cowner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CownerFn)

--------------------------------------------------------------------------------
-- | CsetGlobalDelegationFn
--------------------------------------------------------------------------------


type CsetGlobalDelegationFn = Tagged (SProxy "setGlobalDelegation(address)") (Tuple1 Address)

csetGlobalDelegation :: forall e. TransactionOptions NoPay -> { dlgtAddress :: Address } -> Web3 e HexString
csetGlobalDelegation x0 r = uncurryFields  r $ csetGlobalDelegation' x0
   where
    csetGlobalDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "dlgtAddress") Address -> Web3 e HexString
    csetGlobalDelegation' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CsetGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | CsetTokenDelegationFn
--------------------------------------------------------------------------------


type CsetTokenDelegationFn = Tagged (SProxy "setTokenDelegation(address,address)") (Tuple2 Address Address)

csetTokenDelegation :: forall e. TransactionOptions NoPay -> { tokenContract :: Address, dlgtAddress :: Address } -> Web3 e HexString
csetTokenDelegation x0 r = uncurryFields  r $ csetTokenDelegation' x0
   where
    csetTokenDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "tokenContract") Address -> Tagged (SProxy "dlgtAddress") Address -> Web3 e HexString
    csetTokenDelegation' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CsetTokenDelegationFn)

--------------------------------------------------------------------------------
-- | C_rawGetGlobalDelegationFn
--------------------------------------------------------------------------------


type C_rawGetGlobalDelegationFn = Tagged (SProxy "_rawGetGlobalDelegation(address)") (Tuple1 Address)

c_rawGetGlobalDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { _voter :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
c_rawGetGlobalDelegation x0 cm r = uncurryFields  r $ c_rawGetGlobalDelegation' x0 cm
   where
    c_rawGetGlobalDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_voter") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& D6)) Address (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6))))
    c_rawGetGlobalDelegation' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: C_rawGetGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | CconstructorFn
--------------------------------------------------------------------------------


type CconstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

cconstructor :: forall e. TransactionOptions NoPay -> Web3 e HexString
cconstructor x0 = sendTx x0 ((tagged $ Tuple0 ) :: CconstructorFn)

--------------------------------------------------------------------------------
-- | SetGlobalDelegation
--------------------------------------------------------------------------------


newtype SetGlobalDelegation = SetGlobalDelegation {voter :: Address,delegate :: Address}

derive instance newtypeSetGlobalDelegation :: Newtype SetGlobalDelegation _

instance eventFilterSetGlobalDelegation :: EventFilter SetGlobalDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "80e8ffc3c5dd5acf237f5c6e5855a312b8778e3df8ac7346f51155bcfeacf7cd")]

instance indexedEventSetGlobalDelegation :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "voter") Address) (Tagged (SProxy "delegate") Address)) SetGlobalDelegation where
  isAnonymous _ = false

derive instance genericSetGlobalDelegation :: Generic SetGlobalDelegation _

instance eventGenericSetGlobalDelegationShow :: Show SetGlobalDelegation where
	show = genericShow

instance eventGenericSetGlobalDelegationeq :: Eq SetGlobalDelegation where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetTokenDelegation
--------------------------------------------------------------------------------


newtype SetTokenDelegation = SetTokenDelegation {voter :: Address,tokenContract :: Address,delegate :: Address}

derive instance newtypeSetTokenDelegation :: Newtype SetTokenDelegation _

instance eventFilterSetTokenDelegation :: EventFilter SetTokenDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "74d96c2392d2b95d269942d650f623d0c7fb1f54a58e773709f4284f7b449cd7")]

instance indexedEventSetTokenDelegation :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") Address) (Tagged (SProxy "tokenContract") Address) (Tagged (SProxy "delegate") Address)) SetTokenDelegation where
  isAnonymous _ = false

derive instance genericSetTokenDelegation :: Generic SetTokenDelegation _

instance eventGenericSetTokenDelegationShow :: Show SetTokenDelegation where
	show = genericShow

instance eventGenericSetTokenDelegationeq :: Eq SetTokenDelegation where
	eq = genericEq