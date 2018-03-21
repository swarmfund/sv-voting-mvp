--------------------------------------------------------------------------------
-- | FixedSupplyToken
--------------------------------------------------------------------------------

module SecureVote.Contracts.FixedSupplyToken where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

approve :: forall e. TransactionOptions NoPay -> { _spender :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> Tagged (SProxy "_spender") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    approve' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: ApproveFn)

--------------------------------------------------------------------------------
-- | TotalSupplyFn
--------------------------------------------------------------------------------


type TotalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

totalSupply :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
totalSupply x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalSupplyFn)

--------------------------------------------------------------------------------
-- | TransferFromFn
--------------------------------------------------------------------------------


type TransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 Address Address (UIntN (D2 :& D5 :& D6)))

transferFrom :: forall e. TransactionOptions NoPay -> { _from :: Address, _to :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
transferFrom x0 r = uncurryFields  r $ transferFrom' x0
   where
    transferFrom' :: TransactionOptions NoPay -> Tagged (SProxy "_from") Address -> Tagged (SProxy "_to") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    transferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: TransferFromFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN D8))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

balanceOf :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | TransferFn
--------------------------------------------------------------------------------


type TransferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 Address (UIntN (D2 :& D5 :& D6)))

transfer :: forall e. TransactionOptions NoPay -> { _to :: Address, _amount :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
transfer x0 r = uncurryFields  r $ transfer' x0
   where
    transfer' :: TransactionOptions NoPay -> Tagged (SProxy "_to") Address -> Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: TransferFn)

--------------------------------------------------------------------------------
-- | AllowanceFn
--------------------------------------------------------------------------------


type AllowanceFn = Tagged (SProxy "allowance(address,address)") (Tuple2 Address Address)

allowance :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address, _spender :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
allowance x0 cm r = uncurryFields  r $ allowance' x0 cm
   where
    allowance' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Tagged (SProxy "_spender") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    allowance' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: AllowanceFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

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