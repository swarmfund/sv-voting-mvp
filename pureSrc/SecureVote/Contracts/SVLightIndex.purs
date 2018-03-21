--------------------------------------------------------------------------------
-- | SVLightIndex
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightIndex where

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
-- | CsetOwnerFn
--------------------------------------------------------------------------------


type CsetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 Address)

csetOwner :: forall e. TransactionOptions NoPay -> { _owner :: Address } -> Web3 e HexString
csetOwner x0 r = uncurryFields  r $ csetOwner' x0
   where
    csetOwner' :: TransactionOptions NoPay -> Tagged (SProxy "_owner") Address -> Web3 e HexString
    csetOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CsetOwnerFn)

--------------------------------------------------------------------------------
-- | CsetWhitelistBallotFn
--------------------------------------------------------------------------------


type CsetWhitelistBallotFn = Tagged (SProxy "setWhitelistBallot(address,bool)") (Tuple2 Address Boolean)

csetWhitelistBallot :: forall e. TransactionOptions NoPay -> { addr :: Address, _free :: Boolean } -> Web3 e HexString
csetWhitelistBallot x0 r = uncurryFields  r $ csetWhitelistBallot' x0
   where
    csetWhitelistBallot' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "_free") Boolean -> Web3 e HexString
    csetWhitelistBallot' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CsetWhitelistBallotFn)

--------------------------------------------------------------------------------
-- | CgetDemocInfoFn
--------------------------------------------------------------------------------


type CgetDemocInfoFn = Tagged (SProxy "getDemocInfo(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

cgetDemocInfo :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)) } -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& D6))))
cgetDemocInfo x0 cm r = uncurryFields  r $ cgetDemocInfo' x0 cm
   where
    cgetDemocInfo' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& D6))))
    cgetDemocInfo' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: CgetDemocInfoFn)

--------------------------------------------------------------------------------
-- | CsetPaymentEnabledFn
--------------------------------------------------------------------------------


type CsetPaymentEnabledFn = Tagged (SProxy "setPaymentEnabled(bool)") (Tuple1 Boolean)

csetPaymentEnabled :: forall e. TransactionOptions NoPay -> { _enabled :: Boolean } -> Web3 e HexString
csetPaymentEnabled x0 r = uncurryFields  r $ csetPaymentEnabled' x0
   where
    csetPaymentEnabled' :: TransactionOptions NoPay -> Tagged (SProxy "_enabled") Boolean -> Web3 e HexString
    csetPaymentEnabled' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CsetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | CsetFeeForFn
--------------------------------------------------------------------------------


type CsetFeeForFn = Tagged (SProxy "setFeeFor(address,uint128[2])") (Tuple2 Address (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))

csetFeeFor :: forall e. TransactionOptions NoPay -> { addr :: Address, fees :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) } -> Web3 e HexString
csetFeeFor x0 r = uncurryFields  r $ csetFeeFor' x0
   where
    csetFeeFor' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "fees") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) -> Web3 e HexString
    csetFeeFor' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CsetFeeForFn)

--------------------------------------------------------------------------------
-- | CnDemocsFn
--------------------------------------------------------------------------------


type CnDemocsFn = Tagged (SProxy "nDemocs()") (Tuple0 )

cnDemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
cnDemocs x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CnDemocsFn)

--------------------------------------------------------------------------------
-- | CsetPayToFn
--------------------------------------------------------------------------------


type CsetPayToFn = Tagged (SProxy "setPayTo(address)") (Tuple1 Address)

csetPayTo :: forall e. TransactionOptions NoPay -> { newPayTo :: Address } -> Web3 e HexString
csetPayTo x0 r = uncurryFields  r $ csetPayTo' x0
   where
    csetPayTo' :: TransactionOptions NoPay -> Tagged (SProxy "newPayTo") Address -> Web3 e HexString
    csetPayTo' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CsetPayToFn)

--------------------------------------------------------------------------------
-- | CdeployBallotFn
--------------------------------------------------------------------------------


type CdeployBallotFn = Tagged (SProxy "deployBallot(bytes32,bytes32,bytes32,uint64[2],bool[2])") (Tuple5 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) (Vector (S (S (Z))) (UIntN (D6 :& D4))) (Vector (S (S (Z))) Boolean))

cdeployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& D2)), specHash :: (BytesN (D3 :& D2)), extraData :: (BytesN (D3 :& D2)), openPeriod :: (Vector (S (S (Z))) (UIntN (D6 :& D4))), flags :: (Vector (S (S (Z))) Boolean) } -> Web3 e HexString
cdeployBallot x0 r = uncurryFields  r $ cdeployBallot' x0
   where
    cdeployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& D2)) -> Tagged (SProxy "openPeriod") (Vector (S (S (Z))) (UIntN (D6 :& D4))) -> Tagged (SProxy "flags") (Vector (S (S (Z))) Boolean) -> Web3 e HexString
    cdeployBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: CdeployBallotFn)

--------------------------------------------------------------------------------
-- | CdemocFeeFn
--------------------------------------------------------------------------------


type CdemocFeeFn = Tagged (SProxy "democFee()") (Tuple0 )

cdemocFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& D2 :& D8)))
cdemocFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CdemocFeeFn)

--------------------------------------------------------------------------------
-- | CpaymentEnabledFn
--------------------------------------------------------------------------------


type CpaymentEnabledFn = Tagged (SProxy "paymentEnabled()") (Tuple0 )

cpaymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
cpaymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CpaymentEnabledFn)

--------------------------------------------------------------------------------
-- | CdemocWhitelistFn
--------------------------------------------------------------------------------


type CdemocWhitelistFn = Tagged (SProxy "democWhitelist(address)") (Tuple1 Address)

cdemocWhitelist :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
cdemocWhitelist x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: CdemocWhitelistFn)

--------------------------------------------------------------------------------
-- | CgetNthBallotFn
--------------------------------------------------------------------------------


type CgetNthBallotFn = Tagged (SProxy "getNthBallot(bytes32,uint256)") (Tuple2 (BytesN (D3 :& D2)) (UIntN (D2 :& D5 :& D6)))

cgetNthBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)), n :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e (Either CallError (Tuple4 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address (UIntN (D6 :& D4))))
cgetNthBallot x0 cm r = uncurryFields  r $ cgetNthBallot' x0 cm
   where
    cgetNthBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (Tuple4 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address (UIntN (D6 :& D4))))
    cgetNthBallot' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: CgetNthBallotFn)

--------------------------------------------------------------------------------
-- | CownerFn
--------------------------------------------------------------------------------


type CownerFn = Tagged (SProxy "owner()") (Tuple0 )

cowner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
cowner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CownerFn)

--------------------------------------------------------------------------------
-- | CdemocsFn
--------------------------------------------------------------------------------


type CdemocsFn = Tagged (SProxy "democs(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

cdemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& D2)) -> Web3 e (Either CallError (Tuple2 String Address))
cdemocs x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: CdemocsFn)

--------------------------------------------------------------------------------
-- | CaddBallotFn
--------------------------------------------------------------------------------


type CaddBallotFn = Tagged (SProxy "addBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)) Address)

caddBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& D2)), extraData :: (BytesN (D3 :& D2)), votingContract :: Address } -> Web3 e HexString
caddBallot x0 r = uncurryFields  r $ caddBallot' x0
   where
    caddBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& D2)) -> Tagged (SProxy "votingContract") Address -> Web3 e HexString
    caddBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: CaddBallotFn)

--------------------------------------------------------------------------------
-- | CsetWhitelistDemocFn
--------------------------------------------------------------------------------


type CsetWhitelistDemocFn = Tagged (SProxy "setWhitelistDemoc(address,bool)") (Tuple2 Address Boolean)

csetWhitelistDemoc :: forall e. TransactionOptions NoPay -> { addr :: Address, _free :: Boolean } -> Web3 e HexString
csetWhitelistDemoc x0 r = uncurryFields  r $ csetWhitelistDemoc' x0
   where
    csetWhitelistDemoc' :: TransactionOptions NoPay -> Tagged (SProxy "addr") Address -> Tagged (SProxy "_free") Boolean -> Web3 e HexString
    csetWhitelistDemoc' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CsetWhitelistDemocFn)

--------------------------------------------------------------------------------
-- | CpayToFn
--------------------------------------------------------------------------------


type CpayToFn = Tagged (SProxy "payTo()") (Tuple0 )

cpayTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
cpayTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CpayToFn)

--------------------------------------------------------------------------------
-- | CsetEthFn
--------------------------------------------------------------------------------


type CsetEthFn = Tagged (SProxy "setEth(uint128[2])") (Tuple1 (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))))

csetEth :: forall e. TransactionOptions NoPay -> { newFees :: (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) } -> Web3 e HexString
csetEth x0 r = uncurryFields  r $ csetEth' x0
   where
    csetEth' :: TransactionOptions NoPay -> Tagged (SProxy "newFees") (Vector (S (S (Z))) (UIntN (D1 :& D2 :& D8))) -> Web3 e HexString
    csetEth' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CsetEthFn)

--------------------------------------------------------------------------------
-- | CballotWhitelistFn
--------------------------------------------------------------------------------


type CballotWhitelistFn = Tagged (SProxy "ballotWhitelist(address)") (Tuple1 Address)

cballotWhitelist :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
cballotWhitelist x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: CballotWhitelistFn)

--------------------------------------------------------------------------------
-- | CnBallotsFn
--------------------------------------------------------------------------------


type CnBallotsFn = Tagged (SProxy "nBallots(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

cnBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
cnBallots x0 cm r = uncurryFields  r $ cnBallots' x0 cm
   where
    cnBallots' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
    cnBallots' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: CnBallotsFn)

--------------------------------------------------------------------------------
-- | CballotFeeFn
--------------------------------------------------------------------------------


type CballotFeeFn = Tagged (SProxy "ballotFee()") (Tuple0 )

cballotFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& D2 :& D8)))
cballotFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CballotFeeFn)

--------------------------------------------------------------------------------
-- | CdemocListFn
--------------------------------------------------------------------------------


type CdemocListFn = Tagged (SProxy "democList(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

cdemocList :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
cdemocList x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: CdemocListFn)

--------------------------------------------------------------------------------
-- | CsetAdminFn
--------------------------------------------------------------------------------


type CsetAdminFn = Tagged (SProxy "setAdmin(bytes32,address)") (Tuple2 (BytesN (D3 :& D2)) Address)

csetAdmin :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& D2)), newAdmin :: Address } -> Web3 e HexString
csetAdmin x0 r = uncurryFields  r $ csetAdmin' x0
   where
    csetAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    csetAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CsetAdminFn)

--------------------------------------------------------------------------------
-- | CinitDemocFn
--------------------------------------------------------------------------------


type CinitDemocFn = Tagged (SProxy "initDemoc(string)") (Tuple1 String)

cinitDemoc :: forall e. TransactionOptions Wei -> { democName :: String } -> Web3 e HexString
cinitDemoc x0 r = uncurryFields  r $ cinitDemoc' x0
   where
    cinitDemoc' :: TransactionOptions Wei -> Tagged (SProxy "democName") String -> Web3 e HexString
    cinitDemoc' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: CinitDemocFn)

--------------------------------------------------------------------------------
-- | CconstructorFn
--------------------------------------------------------------------------------


type CconstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

cconstructor :: forall e. TransactionOptions NoPay -> Web3 e HexString
cconstructor x0 = sendTx x0 ((tagged $ Tuple0 ) :: CconstructorFn)

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