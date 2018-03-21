--------------------------------------------------------------------------------
-- | SVLightBallotBox
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightBallotBox where

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
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D5, D6, S, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4, UIntN, Z, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | NVotesCastFn
--------------------------------------------------------------------------------


type NVotesCastFn = Tagged (SProxy "nVotesCast()") (Tuple0 )

nVotesCast :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
nVotesCast x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NVotesCastFn)

--------------------------------------------------------------------------------
-- | DeprecatedFn
--------------------------------------------------------------------------------


type DeprecatedFn = Tagged (SProxy "deprecated()") (Tuple0 )

deprecated :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
deprecated x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DeprecatedFn)

--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 Address)

setOwner :: forall e. TransactionOptions NoPay -> { newOwner :: Address } -> Web3 e HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | CreationBlockFn
--------------------------------------------------------------------------------


type CreationBlockFn = Tagged (SProxy "creationBlock()") (Tuple0 )

creationBlock :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& D4)))
creationBlock x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CreationBlockFn)

--------------------------------------------------------------------------------
-- | BallotEncryptionSeckeyFn
--------------------------------------------------------------------------------


type BallotEncryptionSeckeyFn = Tagged (SProxy "ballotEncryptionSeckey()") (Tuple0 )

ballotEncryptionSeckey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
ballotEncryptionSeckey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BallotEncryptionSeckeyFn)

--------------------------------------------------------------------------------
-- | EndTimeFn
--------------------------------------------------------------------------------


type EndTimeFn = Tagged (SProxy "endTime()") (Tuple0 )

endTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& D4)))
endTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EndTimeFn)

--------------------------------------------------------------------------------
-- | StartingBlockAroundFn
--------------------------------------------------------------------------------


type StartingBlockAroundFn = Tagged (SProxy "startingBlockAround()") (Tuple0 )

startingBlockAround :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& D4)))
startingBlockAround x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StartingBlockAroundFn)

--------------------------------------------------------------------------------
-- | GetEncSeckeyFn
--------------------------------------------------------------------------------


type GetEncSeckeyFn = Tagged (SProxy "getEncSeckey()") (Tuple0 )

getEncSeckey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
getEncSeckey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetEncSeckeyFn)

--------------------------------------------------------------------------------
-- | VoterToBallotIDFn
--------------------------------------------------------------------------------


type VoterToBallotIDFn = Tagged (SProxy "voterToBallotID(address)") (Tuple1 Address)

voterToBallotID :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
voterToBallotID x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: VoterToBallotIDFn)

--------------------------------------------------------------------------------
-- | UseEncryptionFn
--------------------------------------------------------------------------------


type UseEncryptionFn = Tagged (SProxy "useEncryption()") (Tuple0 )

useEncryption :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
useEncryption x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: UseEncryptionFn)

--------------------------------------------------------------------------------
-- | StartTimeFn
--------------------------------------------------------------------------------


type StartTimeFn = Tagged (SProxy "startTime()") (Tuple0 )

startTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& D4)))
startTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StartTimeFn)

--------------------------------------------------------------------------------
-- | SpecHashFn
--------------------------------------------------------------------------------


type SpecHashFn = Tagged (SProxy "specHash()") (Tuple0 )

specHash :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
specHash x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SpecHashFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SetDeprecatedFn
--------------------------------------------------------------------------------


type SetDeprecatedFn = Tagged (SProxy "setDeprecated()") (Tuple0 )

setDeprecated :: forall e. TransactionOptions NoPay -> Web3 e HexString
setDeprecated x0 = sendTx x0 ((tagged $ Tuple0 ) :: SetDeprecatedFn)

--------------------------------------------------------------------------------
-- | BallotMapFn
--------------------------------------------------------------------------------


type BallotMapFn = Tagged (SProxy "ballotMap(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

ballotMap :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& D2)) Address (UIntN (D3 :& D2))))
ballotMap x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: BallotMapFn)

--------------------------------------------------------------------------------
-- | AssociatedPubkeysFn
--------------------------------------------------------------------------------


type AssociatedPubkeysFn = Tagged (SProxy "associatedPubkeys(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

associatedPubkeys :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
associatedPubkeys x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AssociatedPubkeysFn)

--------------------------------------------------------------------------------
-- | SubmitBallotWithPkFn
--------------------------------------------------------------------------------


type SubmitBallotWithPkFn = Tagged (SProxy "submitBallotWithPk(bytes32,bytes32)") (Tuple2 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)))

submitBallotWithPk :: forall e. TransactionOptions NoPay -> { encryptedBallot :: (BytesN (D3 :& D2)), senderPubkey :: (BytesN (D3 :& D2)) } -> Web3 e HexString
submitBallotWithPk x0 r = uncurryFields  r $ submitBallotWithPk' x0
   where
    submitBallotWithPk' :: TransactionOptions NoPay -> Tagged (SProxy "encryptedBallot") (BytesN (D3 :& D2)) -> Tagged (SProxy "senderPubkey") (BytesN (D3 :& D2)) -> Web3 e HexString
    submitBallotWithPk' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SubmitBallotWithPkFn)

--------------------------------------------------------------------------------
-- | RevealSeckeyFn
--------------------------------------------------------------------------------


type RevealSeckeyFn = Tagged (SProxy "revealSeckey(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

revealSeckey :: forall e. TransactionOptions NoPay -> { _secKey :: (BytesN (D3 :& D2)) } -> Web3 e HexString
revealSeckey x0 r = uncurryFields  r $ revealSeckey' x0
   where
    revealSeckey' :: TransactionOptions NoPay -> Tagged (SProxy "_secKey") (BytesN (D3 :& D2)) -> Web3 e HexString
    revealSeckey' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RevealSeckeyFn)

--------------------------------------------------------------------------------
-- | TestModeFn
--------------------------------------------------------------------------------


type TestModeFn = Tagged (SProxy "testMode()") (Tuple0 )

testMode :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
testMode x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TestModeFn)

--------------------------------------------------------------------------------
-- | SubmitBallotNoPkFn
--------------------------------------------------------------------------------


type SubmitBallotNoPkFn = Tagged (SProxy "submitBallotNoPk(bytes32)") (Tuple1 (BytesN (D3 :& D2)))

submitBallotNoPk :: forall e. TransactionOptions NoPay -> { ballot :: (BytesN (D3 :& D2)) } -> Web3 e HexString
submitBallotNoPk x0 r = uncurryFields  r $ submitBallotNoPk' x0
   where
    submitBallotNoPk' :: TransactionOptions NoPay -> Tagged (SProxy "ballot") (BytesN (D3 :& D2)) -> Web3 e HexString
    submitBallotNoPk' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SubmitBallotNoPkFn)

--------------------------------------------------------------------------------
-- | SetEndTimeFn
--------------------------------------------------------------------------------


type SetEndTimeFn = Tagged (SProxy "setEndTime(uint64)") (Tuple1 (UIntN (D6 :& D4)))

setEndTime :: forall e. TransactionOptions NoPay -> { newEndTime :: (UIntN (D6 :& D4)) } -> Web3 e HexString
setEndTime x0 r = uncurryFields  r $ setEndTime' x0
   where
    setEndTime' :: TransactionOptions NoPay -> Tagged (SProxy "newEndTime") (UIntN (D6 :& D4)) -> Web3 e HexString
    setEndTime' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetEndTimeFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(bytes32,uint64[2],bool[2])") (Tuple3 (BytesN (D3 :& D2)) (Vector (S (S (Z))) (UIntN (D6 :& D4))) (Vector (S (S (Z))) Boolean))

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _specHash :: (BytesN (D3 :& D2)), openPeriod :: (Vector (S (S (Z))) (UIntN (D6 :& D4))), flags :: (Vector (S (S (Z))) Boolean) } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_specHash") (BytesN (D3 :& D2)) -> Tagged (SProxy "openPeriod") (Vector (S (S (Z))) (UIntN (D6 :& D4))) -> Tagged (SProxy "flags") (Vector (S (S (Z))) Boolean) -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 = deployContract y0 bc' ((tagged $ Tuple3 (untagged y2 ) (untagged y3 ) (untagged y4 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | CreatedBallot
--------------------------------------------------------------------------------


newtype CreatedBallot = CreatedBallot {_creator :: Address,_openPeriod :: (Vector (S (S (Z))) (UIntN (D6 :& D4))),_useEncryption :: Boolean,_specHash :: (BytesN (D3 :& D2))}

derive instance newtypeCreatedBallot :: Newtype CreatedBallot _

instance eventFilterCreatedBallot :: EventFilter CreatedBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "72e9f65ccb8c470c69c19a96f9ebf2ecfda0034e60416b28bdc6f25cce1d244d")]

instance indexedEventCreatedBallot :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "_creator") Address) (Tagged (SProxy "_openPeriod") (Vector (S (S (Z))) (UIntN (D6 :& D4)))) (Tagged (SProxy "_useEncryption") Boolean) (Tagged (SProxy "_specHash") (BytesN (D3 :& D2)))) CreatedBallot where
  isAnonymous _ = false

derive instance genericCreatedBallot :: Generic CreatedBallot _

instance eventGenericCreatedBallotShow :: Show CreatedBallot where
	show = genericShow

instance eventGenericCreatedBalloteq :: Eq CreatedBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SuccessfulPkVote
--------------------------------------------------------------------------------


newtype SuccessfulPkVote = SuccessfulPkVote {voter :: Address,ballot :: (BytesN (D3 :& D2)),pubkey :: (BytesN (D3 :& D2))}

derive instance newtypeSuccessfulPkVote :: Newtype SuccessfulPkVote _

instance eventFilterSuccessfulPkVote :: EventFilter SuccessfulPkVote where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "9368bc78a0bff8558a96f0716c502e7826c09faab0999d15c1464d46432866ec")]

instance indexedEventSuccessfulPkVote :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") Address) (Tagged (SProxy "ballot") (BytesN (D3 :& D2))) (Tagged (SProxy "pubkey") (BytesN (D3 :& D2)))) SuccessfulPkVote where
  isAnonymous _ = false

derive instance genericSuccessfulPkVote :: Generic SuccessfulPkVote _

instance eventGenericSuccessfulPkVoteShow :: Show SuccessfulPkVote where
	show = genericShow

instance eventGenericSuccessfulPkVoteeq :: Eq SuccessfulPkVote where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SuccessfulVote
--------------------------------------------------------------------------------


newtype SuccessfulVote = SuccessfulVote {voter :: Address,ballot :: (BytesN (D3 :& D2))}

derive instance newtypeSuccessfulVote :: Newtype SuccessfulVote _

instance eventFilterSuccessfulVote :: EventFilter SuccessfulVote where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "620a05d88e802eea13268414c30858ec8b2524d13ee3e3e93519ac140047eba4")]

instance indexedEventSuccessfulVote :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "voter") Address) (Tagged (SProxy "ballot") (BytesN (D3 :& D2)))) SuccessfulVote where
  isAnonymous _ = false

derive instance genericSuccessfulVote :: Generic SuccessfulVote _

instance eventGenericSuccessfulVoteShow :: Show SuccessfulVote where
	show = genericShow

instance eventGenericSuccessfulVoteeq :: Eq SuccessfulVote where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SeckeyRevealed
--------------------------------------------------------------------------------


newtype SeckeyRevealed = SeckeyRevealed {secretKey :: (BytesN (D3 :& D2))}

derive instance newtypeSeckeyRevealed :: Newtype SeckeyRevealed _

instance eventFilterSeckeyRevealed :: EventFilter SeckeyRevealed where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a69839328d982396193483f2260936b1d1f2109fdde204b27c7ac3c1cfd18db0")]

instance indexedEventSeckeyRevealed :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "secretKey") (BytesN (D3 :& D2)))) SeckeyRevealed where
  isAnonymous _ = false

derive instance genericSeckeyRevealed :: Generic SeckeyRevealed _

instance eventGenericSeckeyRevealedShow :: Show SeckeyRevealed where
	show = genericShow

instance eventGenericSeckeyRevealedeq :: Eq SeckeyRevealed where
	eq = genericEq

--------------------------------------------------------------------------------
-- | TestingEnabled
--------------------------------------------------------------------------------


newtype TestingEnabled = TestingEnabled {}

derive instance newtypeTestingEnabled :: Newtype TestingEnabled _

instance eventFilterTestingEnabled :: EventFilter TestingEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "641e6b9d2f3c463bec5b5cffe3f5017d9a49ad5543d2962eb746c6a7afa223c5")]

instance indexedEventTestingEnabled :: IndexedEvent (Tuple0 ) (Tuple0 ) TestingEnabled where
  isAnonymous _ = false

derive instance genericTestingEnabled :: Generic TestingEnabled _

instance eventGenericTestingEnabledShow :: Show TestingEnabled where
	show = genericShow

instance eventGenericTestingEnabledeq :: Eq TestingEnabled where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Error
--------------------------------------------------------------------------------


newtype Error = Error {error :: String}

derive instance newtypeError :: Newtype Error _

instance eventFilterError :: EventFilter Error where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "08c379a0afcc32b1a39302f7cb8073359698411ab5fd6e3edb2c02c0b5fba8aa")]

instance indexedEventError :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "error") String)) Error where
  isAnonymous _ = false

derive instance genericError :: Generic Error _

instance eventGenericErrorShow :: Show Error where
	show = genericShow

instance eventGenericErroreq :: Eq Error where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DeprecatedContract
--------------------------------------------------------------------------------


newtype DeprecatedContract = DeprecatedContract {}

derive instance newtypeDeprecatedContract :: Newtype DeprecatedContract _

instance eventFilterDeprecatedContract :: EventFilter DeprecatedContract where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "77563e26f751f6c469d11286ef3f15cb0d2033a8b182387a2a44782019961787")]

instance indexedEventDeprecatedContract :: IndexedEvent (Tuple0 ) (Tuple0 ) DeprecatedContract where
  isAnonymous _ = false

derive instance genericDeprecatedContract :: Generic DeprecatedContract _

instance eventGenericDeprecatedContractShow :: Show DeprecatedContract where
	show = genericShow

instance eventGenericDeprecatedContracteq :: Eq DeprecatedContract where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetOwner
--------------------------------------------------------------------------------


newtype SetOwner = SetOwner {_owner :: Address}

derive instance newtypeSetOwner :: Newtype SetOwner _

instance eventFilterSetOwner :: EventFilter SetOwner where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "167d3e9c1016ab80e58802ca9da10ce5c6a0f4debc46a2e7a2cd9e56899a4fb5")]

instance indexedEventSetOwner :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_owner") Address)) SetOwner where
  isAnonymous _ = false

derive instance genericSetOwner :: Generic SetOwner _

instance eventGenericSetOwnerShow :: Show SetOwner where
	show = genericShow

instance eventGenericSetOwnereq :: Eq SetOwner where
	eq = genericEq