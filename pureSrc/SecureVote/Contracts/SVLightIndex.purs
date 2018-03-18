--------------------------------------------------------------------------------
-- | SVLightIndex
--------------------------------------------------------------------------------

module SecureVote/Contracts.SVLightIndex where

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
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, D8, S, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4, Tuple5(..), UIntN, Z, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 Address)

setOwner :: forall e. TransactionOptions NoPay -> { _owner :: Address } -> Web3 e HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> Tagged (SProxy "_owner") Address -> Web3 e HexString
    setOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | SetWhitelistBallotFn
--------------------------------------------------------------------------------


type SetWhitelistBallotFn = Tagged (SProxy "setWhitelistBallot(address,bool)") (Tuple2 Address Boolean)

setWhitelistBallot :: forall e. TransactionOptions NoPay -> { addr :: Address, _free :: Boolean } -> Web3 e HexString
setWhitelistBallot x0 r = uncurryFields  r $ setWhitelistBallot' x0
   where
    setWhitelistBallot' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "_free") Boolean -> Web3 e HexString
    setWhitelistBallot' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetWhitelistBallotFn)

--------------------------------------------------------------------------------
-- | GetDemocInfoFn
--------------------------------------------------------------------------------


type GetDemocInfoFn = Tagged (SProxy "getDemocInfo(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

getDemocInfo :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)) } -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& D6))))
getDemocInfo x0 cm r = uncurryFields  r $ getDemocInfo' x0 cm
   where
    getDemocInfo' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& D6))))
    getDemocInfo' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocInfoFn)

--------------------------------------------------------------------------------
-- | SetPaymentEnabledFn
--------------------------------------------------------------------------------


type SetPaymentEnabledFn = Tagged (SProxy "setPaymentEnabled(bool)") (Tuple1 Boolean)

setPaymentEnabled :: forall e. TransactionOptions NoPay -> { _enabled :: Boolean } -> Web3 e HexString
setPaymentEnabled x0 r = uncurryFields  r $ setPaymentEnabled' x0
   where
    setPaymentEnabled' :: TransactionOptions NoPay -> Tagged (SProxy "_enabled") Boolean -> Web3 e HexString
    setPaymentEnabled' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | SetFeeForFn
--------------------------------------------------------------------------------


type SetFeeForFn = Tagged (SProxy "setFeeFor(address,uint128[2])") (Tuple2 Address (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))

setFeeFor :: forall e. TransactionOptions NoPay -> { addr :: Address, fees :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) } -> Web3 e HexString
setFeeFor x0 r = uncurryFields  r $ setFeeFor' x0
   where
    setFeeFor' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "fees") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) -> Web3 e HexString
    setFeeFor' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetFeeForFn)

--------------------------------------------------------------------------------
-- | NDemocsFn
--------------------------------------------------------------------------------


type NDemocsFn = Tagged (SProxy "nDemocs()") (Tuple0 )

nDemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
nDemocs x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NDemocsFn)

--------------------------------------------------------------------------------
-- | SetPayToFn
--------------------------------------------------------------------------------


type SetPayToFn = Tagged (SProxy "setPayTo(address)") (Tuple1 Address)

setPayTo :: forall e. TransactionOptions NoPay -> { newPayTo :: Address } -> Web3 e HexString
setPayTo x0 r = uncurryFields  r $ setPayTo' x0
   where
    setPayTo' :: TransactionOptions NoPay -> Tagged (SProxy "newPayTo") Address -> Web3 e HexString
    setPayTo' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPayToFn)

--------------------------------------------------------------------------------
-- | DeployBallotFn
--------------------------------------------------------------------------------


type DeployBallotFn = Tagged (SProxy "deployBallot(bytes32,bytes32,bytes32,uint64[2],bool[2])") (Tuple5 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) (Vector (S (S (Z))) (UIntN (D6 :& D4))) (Vector (S (S (Z))) Boolean))

deployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& D2)), specHash :: (BytesN (D3 :& D2)), extraData :: (BytesN (D3 :& D2)), openPeriod :: (Vector (S (S (Z))) (UIntN (D6 :& D4))), flags :: (Vector (S (S (Z))) Boolean) } -> Web3 e HexString
deployBallot x0 r = uncurryFields  r $ deployBallot' x0
   where
    deployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& D2)) -> Tagged (SProxy "openPeriod") (Vector (S (S (Z))) (UIntN (D6 :& D4))) -> Tagged (SProxy "flags") (Vector (S (S (Z))) Boolean) -> Web3 e HexString
    deployBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: DeployBallotFn)

--------------------------------------------------------------------------------
-- | DemocFeeFn
--------------------------------------------------------------------------------


type DemocFeeFn = Tagged (SProxy "democFee()") (Tuple0 )

democFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& D2 :& D8)))
democFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DemocFeeFn)

--------------------------------------------------------------------------------
-- | PaymentEnabledFn
--------------------------------------------------------------------------------


type PaymentEnabledFn = Tagged (SProxy "paymentEnabled()") (Tuple0 )

paymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
paymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PaymentEnabledFn)

--------------------------------------------------------------------------------
-- | DemocWhitelistFn
--------------------------------------------------------------------------------


type DemocWhitelistFn = Tagged (SProxy "democWhitelist(address)") (Tuple1 Address)

democWhitelist :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
democWhitelist x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DemocWhitelistFn)

--------------------------------------------------------------------------------
-- | GetNthBallotFn
--------------------------------------------------------------------------------


type GetNthBallotFn = Tagged (SProxy "getNthBallot(bytes32,uint256)") (Tuple2 (BytesN (D3 :& D2)) (UIntN (D2 :& D5 :& D6)))

getNthBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)), n :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e (Either CallError (Tuple4 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address (UIntN (D6 :& D4))))
getNthBallot x0 cm r = uncurryFields  r $ getNthBallot' x0 cm
   where
    getNthBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (Tuple4 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address (UIntN (D6 :& D4))))
    getNthBallot' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetNthBallotFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | DemocsFn
--------------------------------------------------------------------------------


type DemocsFn = Tagged (SProxy "democs(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

democs :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& D2)) -> Web3 e (Either CallError (Tuple2 String Address))
democs x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: DemocsFn)

--------------------------------------------------------------------------------
-- | AddBallotFn
--------------------------------------------------------------------------------


type AddBallotFn = Tagged (SProxy "addBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address)

addBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& D2)), extraData :: (BytesN (D3 :& D2)), votingContract :: Address } -> Web3 e HexString
addBallot x0 r = uncurryFields  r $ addBallot' x0
   where
    addBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& D2)) -> Tagged (SProxy "votingContract") Address -> Web3 e HexString
    addBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: AddBallotFn)

--------------------------------------------------------------------------------
-- | SetWhitelistDemocFn
--------------------------------------------------------------------------------


type SetWhitelistDemocFn = Tagged (SProxy "setWhitelistDemoc(address,bool)") (Tuple2 Address Boolean)

setWhitelistDemoc :: forall e. TransactionOptions NoPay -> { addr :: Address, _free :: Boolean } -> Web3 e HexString
setWhitelistDemoc x0 r = uncurryFields  r $ setWhitelistDemoc' x0
   where
    setWhitelistDemoc' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "_free") Boolean -> Web3 e HexString
    setWhitelistDemoc' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetWhitelistDemocFn)

--------------------------------------------------------------------------------
-- | PayToFn
--------------------------------------------------------------------------------


type PayToFn = Tagged (SProxy "payTo()") (Tuple0 )

payTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
payTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PayToFn)

--------------------------------------------------------------------------------
-- | SetEthFn
--------------------------------------------------------------------------------


type SetEthFn = Tagged (SProxy "setEth(uint128[2])") (Tuple1 (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))

setEth :: forall e. TransactionOptions NoPay -> { newFees :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) } -> Web3 e HexString
setEth x0 r = uncurryFields  r $ setEth' x0
   where
    setEth' :: TransactionOptions NoPay -> Tagged (SProxy "newFees") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) -> Web3 e HexString
    setEth' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetEthFn)

--------------------------------------------------------------------------------
-- | BallotWhitelistFn
--------------------------------------------------------------------------------


type BallotWhitelistFn = Tagged (SProxy "ballotWhitelist(address)") (Tuple1 Address)

ballotWhitelist :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
ballotWhitelist x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BallotWhitelistFn)

--------------------------------------------------------------------------------
-- | NBallotsFn
--------------------------------------------------------------------------------


type NBallotsFn = Tagged (SProxy "nBallots(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

nBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
nBallots x0 cm r = uncurryFields  r $ nBallots' x0 cm
   where
    nBallots' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    nBallots' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: NBallotsFn)

--------------------------------------------------------------------------------
-- | BallotFeeFn
--------------------------------------------------------------------------------


type BallotFeeFn = Tagged (SProxy "ballotFee()") (Tuple0 )

ballotFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& D2 :& D8)))
ballotFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BallotFeeFn)

--------------------------------------------------------------------------------
-- | DemocListFn
--------------------------------------------------------------------------------


type DemocListFn = Tagged (SProxy "democList(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

democList :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
democList x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DemocListFn)

--------------------------------------------------------------------------------
-- | SetAdminFn
--------------------------------------------------------------------------------


type SetAdminFn = Tagged (SProxy "setAdmin(bytes32,address)") (Tuple2 (BytesN (D3 :& D2)) Address)

setAdmin :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& D2)), newAdmin :: Address } -> Web3 e HexString
setAdmin x0 r = uncurryFields  r $ setAdmin' x0
   where
    setAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    setAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetAdminFn)

--------------------------------------------------------------------------------
-- | InitDemocFn
--------------------------------------------------------------------------------


type InitDemocFn = Tagged (SProxy "initDemoc(string)") (Tuple1 String)

initDemoc :: forall e. TransactionOptions Wei -> { democName :: String } -> Web3 e HexString
initDemoc x0 r = uncurryFields  r $ initDemoc' x0
   where
    initDemoc' :: TransactionOptions Wei -> Tagged (SProxy "democName") String -> Web3 e HexString
    initDemoc' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: InitDemocFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> Web3 e HexString
constructor x0 = sendTx x0 ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PaymentMade
--------------------------------------------------------------------------------


newtype PaymentMade = PaymentMade {valAndRemainder :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8)))}

derive instance newtypePaymentMade :: Newtype PaymentMade _

instance eventFilterPaymentMade :: EventFilter PaymentMade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "02c72dcc40109684591df90d749dbf94cfcc74677fac14e9b0d571b660484fca")]

instance indexedEventPaymentMade :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "valAndRemainder") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))) PaymentMade where
  isAnonymous _ = false

derive instance genericPaymentMade :: Generic PaymentMade _

instance eventGenericPaymentMadeShow :: Show PaymentMade where
	show = genericShow

instance eventGenericPaymentMadeeq :: Eq PaymentMade where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocInit
--------------------------------------------------------------------------------


newtype DemocInit = DemocInit {name :: String,democHash :: (BytesN (D3 :& D2)),admin :: Address}

derive instance newtypeDemocInit :: Newtype DemocInit _

instance eventFilterDemocInit :: EventFilter DemocInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e4ce3800aac9ab0f2b800a84893f01077fec89d059a04ba63cfe5851eebf75bf")]

instance indexedEventDemocInit :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "name") String) (Tagged (SProxy "democHash") (BytesN (D3 :& D2))) (Tagged (SProxy "admin") Address)) DemocInit where
  isAnonymous _ = false

derive instance genericDemocInit :: Generic DemocInit _

instance eventGenericDemocInitShow :: Show DemocInit where
	show = genericShow

instance eventGenericDemocIniteq :: Eq DemocInit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotInit
--------------------------------------------------------------------------------


newtype BallotInit = BallotInit {specHash :: (BytesN (D3 :& D2)),openPeriod :: (Vector (S (S (Z))) (UIntN (D6 :& D4))),flags :: (Vector (S (S (Z))) Boolean)}

derive instance newtypeBallotInit :: Newtype BallotInit _

instance eventFilterBallotInit :: EventFilter BallotInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "bc7a1a229e81699a6e3aeb6f23b8ab68c29ac40d8943fe0f763978cd37fbd946")]

instance indexedEventBallotInit :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "specHash") (BytesN (D3 :& D2))) (Tagged (SProxy "openPeriod") (Vector (S (S (Z))) (UIntN (D6 :& D4)))) (Tagged (SProxy "flags") (Vector (S (S (Z))) Boolean))) BallotInit where
  isAnonymous _ = false

derive instance genericBallotInit :: Generic BallotInit _

instance eventGenericBallotInitShow :: Show BallotInit where
	show = genericShow

instance eventGenericBallotIniteq :: Eq BallotInit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotAdded
--------------------------------------------------------------------------------


newtype BallotAdded = BallotAdded {democHash :: (BytesN (D3 :& D2)),specHash :: (BytesN (D3 :& D2)),extraData :: (BytesN (D3 :& D2)),votingContract :: Address}

derive instance newtypeBallotAdded :: Newtype BallotAdded _

instance eventFilterBallotAdded :: EventFilter BallotAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "cd67d56088f4b97d8a591f2f1ba8f11349d19c0f4be8d29bef80df49372fdc46")]

instance indexedEventBallotAdded :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "democHash") (BytesN (D3 :& D2))) (Tagged (SProxy "specHash") (BytesN (D3 :& D2))) (Tagged (SProxy "extraData") (BytesN (D3 :& D2))) (Tagged (SProxy "votingContract") Address)) BallotAdded where
  isAnonymous _ = false

derive instance genericBallotAdded :: Generic BallotAdded _

instance eventGenericBallotAddedShow :: Show BallotAdded where
	show = genericShow

instance eventGenericBallotAddedeq :: Eq BallotAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetFees
--------------------------------------------------------------------------------


newtype SetFees = SetFees {_newFees :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8)))}

derive instance newtypeSetFees :: Newtype SetFees _

instance eventFilterSetFees :: EventFilter SetFees where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b50669505fae6849285fbf48e490e26a77ecc9929129c80d25cd2043d0ba1444")]

instance indexedEventSetFees :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_newFees") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))) SetFees where
  isAnonymous _ = false

derive instance genericSetFees :: Generic SetFees _

instance eventGenericSetFeesShow :: Show SetFees where
	show = genericShow

instance eventGenericSetFeeseq :: Eq SetFees where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PaymentEnabled
--------------------------------------------------------------------------------


newtype PaymentEnabled = PaymentEnabled {_feeEnabled :: Boolean}

derive instance newtypePaymentEnabled :: Newtype PaymentEnabled _

instance eventFilterPaymentEnabled :: EventFilter PaymentEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ed53661a07b1eecfd9ce68c3067068f29dc710c7144b27617faed07dbec21b90")]

instance indexedEventPaymentEnabled :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_feeEnabled") Boolean)) PaymentEnabled where
  isAnonymous _ = false

derive instance genericPaymentEnabled :: Generic PaymentEnabled _

instance eventGenericPaymentEnabledShow :: Show PaymentEnabled where
	show = genericShow

instance eventGenericPaymentEnabledeq :: Eq PaymentEnabled where
	eq = genericEq