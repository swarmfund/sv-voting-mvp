--------------------------------------------------------------------------------
-- | FakeErc20
--------------------------------------------------------------------------------

module SecureVote.Contracts.FakeErc20 where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | CnameFn
--------------------------------------------------------------------------------


type CnameFn = Tagged (SProxy "name()") (Tuple0 )

cname :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
cname x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CnameFn)

--------------------------------------------------------------------------------
-- | CapproveFn
--------------------------------------------------------------------------------


type CapproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

capprove :: forall e. TransactionOptions NoPay -> { _spender :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
capprove x0 r = uncurryFields  r $ capprove' x0
   where
    capprove' :: TransactionOptions NoPay -> Tagged (SProxy "_spender") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    capprove' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CapproveFn)

--------------------------------------------------------------------------------
-- | CtotalSupplyFn
--------------------------------------------------------------------------------


type CtotalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

ctotalSupply :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
ctotalSupply x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CtotalSupplyFn)

--------------------------------------------------------------------------------
-- | CtransferFromFn
--------------------------------------------------------------------------------


type CtransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 Address Address (UIntN (D2 :& D5 :& D6)))

ctransferFrom :: forall e. TransactionOptions NoPay -> { _from :: Address, _to :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
ctransferFrom x0 r = uncurryFields  r $ ctransferFrom' x0
   where
    ctransferFrom' :: TransactionOptions NoPay -> Tagged (SProxy "_from") Address -> Tagged (SProxy "_to") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    ctransferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: CtransferFromFn)

--------------------------------------------------------------------------------
-- | CdecimalsFn
--------------------------------------------------------------------------------


type CdecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

cdecimals :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN D8))
cdecimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CdecimalsFn)

--------------------------------------------------------------------------------
-- | CbalanceOfFn
--------------------------------------------------------------------------------


type CbalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

cbalanceOf :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
cbalanceOf x0 cm r = uncurryFields  r $ cbalanceOf' x0 cm
   where
    cbalanceOf' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    cbalanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: CbalanceOfFn)

--------------------------------------------------------------------------------
-- | CownerFn
--------------------------------------------------------------------------------


type CownerFn = Tagged (SProxy "owner()") (Tuple0 )

cowner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
cowner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CownerFn)

--------------------------------------------------------------------------------
-- | CsymbolFn
--------------------------------------------------------------------------------


type CsymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

csymbol :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
csymbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CsymbolFn)

--------------------------------------------------------------------------------
-- | CtransferFn
--------------------------------------------------------------------------------


type CtransferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

ctransfer :: forall e. TransactionOptions NoPay -> { _to :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
ctransfer x0 r = uncurryFields  r $ ctransfer' x0
   where
    ctransfer' :: TransactionOptions NoPay -> Tagged (SProxy "_to") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    ctransfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CtransferFn)

--------------------------------------------------------------------------------
-- | CallowanceFn
--------------------------------------------------------------------------------


type CallowanceFn = Tagged (SProxy "allowance(address,address)") (Tuple2 Address Address)

callowance :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address, _spender :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
callowance x0 cm r = uncurryFields  r $ callowance' x0 cm
   where
    callowance' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Tagged (SProxy "_spender") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    callowance' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: CallowanceFn)

--------------------------------------------------------------------------------
-- | CconstructorFn
--------------------------------------------------------------------------------


type CconstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

cconstructor :: forall e. TransactionOptions NoPay -> Web3 e HexString
cconstructor x0 = sendTx x0 ((tagged $ Tuple0 ) :: CconstructorFn)

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_value :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
	show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_spender :: Address,_value :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_spender") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
	show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
	eq = genericEq