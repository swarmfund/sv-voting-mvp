--------------------------------------------------------------------------------
-- | SwarmVotingMVP
--------------------------------------------------------------------------------

module SecureVote/Contracts.SwarmVotingMVP where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, S, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple9(..), UIntN, Z, class IndexedEvent, unTuple1)
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
-- | GetBallotOptionsFn
--------------------------------------------------------------------------------


type GetBallotOptionsFn = Tagged (SProxy "getBallotOptions()") (Tuple0 )

getBallotOptions :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (Vector (S (S (S (S (S (Z)))))) (BytesN (D3 :& D2))))
getBallotOptions x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBallotOptionsFn)

--------------------------------------------------------------------------------
-- | SubmitBallotFn
--------------------------------------------------------------------------------


type SubmitBallotFn = Tagged (SProxy "submitBallot(bytes32,bytes32)") (Tuple2 (BytesN (D3 :& D2)) (BytesN (D3 :& D2)))

submitBallot :: forall e. TransactionOptions NoPay -> { encryptedBallot :: (BytesN (D3 :& D2)), senderPubkey :: (BytesN (D3 :& D2)) } -> Web3 e HexString
submitBallot x0 r = uncurryFields  r $ submitBallot' x0
   where
    submitBallot' :: TransactionOptions NoPay -> Tagged (SProxy "encryptedBallot") (BytesN (D3 :& D2)) -> Tagged (SProxy "senderPubkey") (BytesN (D3 :& D2)) -> Web3 e HexString
    submitBallot' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SubmitBallotFn)

--------------------------------------------------------------------------------
-- | BallotEncryptionPubkeyFn
--------------------------------------------------------------------------------


type BallotEncryptionPubkeyFn = Tagged (SProxy "ballotEncryptionPubkey()") (Tuple0 )

ballotEncryptionPubkey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
ballotEncryptionPubkey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BallotEncryptionPubkeyFn)

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

endTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
endTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EndTimeFn)

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
-- | GetBallotOptNumberFn
--------------------------------------------------------------------------------


type GetBallotOptNumberFn = Tagged (SProxy "getBallotOptNumber()") (Tuple0 )

getBallotOptNumber :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
getBallotOptNumber x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBallotOptNumberFn)

--------------------------------------------------------------------------------
-- | BannedAddressesFn
--------------------------------------------------------------------------------


type BannedAddressesFn = Tagged (SProxy "bannedAddresses(address)") (Tuple1 Address)

bannedAddresses :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
bannedAddresses x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BannedAddressesFn)

--------------------------------------------------------------------------------
-- | AssociatedAddressesFn
--------------------------------------------------------------------------------


type AssociatedAddressesFn = Tagged (SProxy "associatedAddresses(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

associatedAddresses :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError Address)
associatedAddresses x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AssociatedAddressesFn)

--------------------------------------------------------------------------------
-- | SwarmFundAddressFn
--------------------------------------------------------------------------------


type SwarmFundAddressFn = Tagged (SProxy "swarmFundAddress()") (Tuple0 )

swarmFundAddress :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
swarmFundAddress x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SwarmFundAddressFn)

--------------------------------------------------------------------------------
-- | GetEncPubkeyFn
--------------------------------------------------------------------------------


type GetEncPubkeyFn = Tagged (SProxy "getEncPubkey()") (Tuple0 )

getEncPubkey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& D2)))
getEncPubkey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetEncPubkeyFn)

--------------------------------------------------------------------------------
-- | EncryptedBallotsFn
--------------------------------------------------------------------------------


type EncryptedBallotsFn = Tagged (SProxy "encryptedBallots(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

encryptedBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
encryptedBallots x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: EncryptedBallotsFn)

--------------------------------------------------------------------------------
-- | StartTimeFn
--------------------------------------------------------------------------------


type StartTimeFn = Tagged (SProxy "startTime()") (Tuple0 )

startTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& D6)))
startTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StartTimeFn)

--------------------------------------------------------------------------------
-- | BanAddressFn
--------------------------------------------------------------------------------


type BanAddressFn = Tagged (SProxy "banAddress(address)") (Tuple1 Address)

banAddress :: forall e. TransactionOptions NoPay -> { _addr :: Address } -> Web3 e HexString
banAddress x0 r = uncurryFields  r $ banAddress' x0
   where
    banAddress' :: TransactionOptions NoPay -> Tagged (SProxy "_addr") Address -> Web3 e HexString
    banAddress' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: BanAddressFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | OptionHashesFn
--------------------------------------------------------------------------------


type OptionHashesFn = Tagged (SProxy "optionHashes(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

optionHashes :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
optionHashes x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: OptionHashesFn)

--------------------------------------------------------------------------------
-- | AssociatedPubkeysFn
--------------------------------------------------------------------------------


type AssociatedPubkeysFn = Tagged (SProxy "associatedPubkeys(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

associatedPubkeys :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError (BytesN (D3 :& D2)))
associatedPubkeys x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AssociatedPubkeysFn)

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
-- | SetEndTimeFn
--------------------------------------------------------------------------------


type SetEndTimeFn = Tagged (SProxy "setEndTime(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

setEndTime :: forall e. TransactionOptions NoPay -> { newEndTime :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
setEndTime x0 r = uncurryFields  r $ setEndTime' x0
   where
    setEndTime' :: TransactionOptions NoPay -> Tagged (SProxy "newEndTime") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    setEndTime' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetEndTimeFn)

--------------------------------------------------------------------------------
-- | TestModeFn
--------------------------------------------------------------------------------


type TestModeFn = Tagged (SProxy "testMode()") (Tuple0 )

testMode :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
testMode x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TestModeFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(uint256,uint256,bytes32,bool,string,string,string,string,string)") (Tuple9 (UIntN (D2 :& D5 :& D6)) (UIntN (D2 :& D5 :& D6)) (BytesN (D3 :& D2)) Boolean String String String String String)

constructor :: forall e. TransactionOptions NoPay -> { _startTime :: (UIntN (D2 :& D5 :& D6)), _endTime :: (UIntN (D2 :& D5 :& D6)), _encPK :: (BytesN (D3 :& D2)), enableTesting :: Boolean, opt1 :: String, opt2 :: String, opt3 :: String, opt4 :: String, opt5 :: String } -> Web3 e HexString
constructor x0 r = uncurryFields  r $ constructor' x0
   where
    constructor' :: TransactionOptions NoPay -> Tagged (SProxy "_startTime") (UIntN (D2 :& D5 :& D6)) -> Tagged (SProxy "_endTime") (UIntN (D2 :& D5 :& D6)) -> Tagged (SProxy "_encPK") (BytesN (D3 :& D2)) -> Tagged (SProxy "enableTesting") Boolean -> Tagged (SProxy "opt1") String -> Tagged (SProxy "opt2") String -> Tagged (SProxy "opt3") String -> Tagged (SProxy "opt4") String -> Tagged (SProxy "opt5") String -> Web3 e HexString
    constructor' y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 = sendTx y0 ((tagged $ Tuple9 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 ) (untagged y6 ) (untagged y7 ) (untagged y8 ) (untagged y9 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | CreatedBallot
--------------------------------------------------------------------------------


newtype CreatedBallot = CreatedBallot {creator :: Address,start :: (UIntN (D2 :& D5 :& D6)),end :: (UIntN (D2 :& D5 :& D6)),encPubkey :: (BytesN (D3 :& D2)),o1 :: String,o2 :: String,o3 :: String,o4 :: String,o5 :: String}

derive instance newtypeCreatedBallot :: Newtype CreatedBallot _

instance eventFilterCreatedBallot :: EventFilter CreatedBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "57397edddef053a4f5975e0a8076e912888e1332721804f7639193b01ef88b7c")]

instance indexedEventCreatedBallot :: IndexedEvent (Tuple0 ) (Tuple9 (Tagged (SProxy "creator") Address) (Tagged (SProxy "start") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "end") (UIntN (D2 :& D5 :& D6))) (Tagged (SProxy "encPubkey") (BytesN (D3 :& D2))) (Tagged (SProxy "o1") String) (Tagged (SProxy "o2") String) (Tagged (SProxy "o3") String) (Tagged (SProxy "o4") String) (Tagged (SProxy "o5") String)) CreatedBallot where
  isAnonymous _ = false

derive instance genericCreatedBallot :: Generic CreatedBallot _

instance eventGenericCreatedBallotShow :: Show CreatedBallot where
	show = genericShow

instance eventGenericCreatedBalloteq :: Eq CreatedBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SuccessfulVote
--------------------------------------------------------------------------------


newtype SuccessfulVote = SuccessfulVote {voter :: Address,ballot :: (BytesN (D3 :& D2)),pubkey :: (BytesN (D3 :& D2))}

derive instance newtypeSuccessfulVote :: Newtype SuccessfulVote _

instance eventFilterSuccessfulVote :: EventFilter SuccessfulVote where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1d69ccdccc669e38a717d9fff71b324d95835c150aa4e2d8900cebd134efff6c")]

instance indexedEventSuccessfulVote :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") Address) (Tagged (SProxy "ballot") (BytesN (D3 :& D2))) (Tagged (SProxy "pubkey") (BytesN (D3 :& D2)))) SuccessfulVote where
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