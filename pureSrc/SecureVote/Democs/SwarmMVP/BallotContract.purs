module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Crypt.NaCl (toUint8Array)
import Data.Array (head, tail, (:))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5, runFn6)
import Data.Int (decimal, fromStringAs, toNumber, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Math (round)
import SecureVote.Crypto.Curve25519 (decryptOneTimeBallot, toBox, toBoxPubkey, toBoxSeckey)
import SecureVote.Utils.ArrayBuffer (fromEthHex, fromEthHexE, fromHex, fromHexE)
import SecureVote.Utils.Monads (mToE)
import SecureVote.Utils.Poly (self)
import SecureVote.Utils.Time (currentTimestamp)
import Unsafe.Coerce (unsafeCoerce)


data SwmVotingContract = SwmVotingContract SwmVotingContract


type BallotResult = Either String String


type Ballot = {ballot :: Uint8Array, pubkey :: Uint8Array, address :: String}
type EncBallot = {encBallot :: Uint8Array, pubkey :: Uint8Array, address :: String}


noArgs :: Unit
noArgs = unit


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import setWeb3ProviderImpl :: forall a. Fn1 String a

-- web3 FFI
foreign import getAccountImpl :: forall a b. Fn3 (a -> Either a b) (b -> Either a b) Int (Either String String)

-- contract FFI
foreign import getBallotSKImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) SwmVotingContract (Maybe a)
foreign import getBallotPropImpl :: forall a b c. Fn5 (a -> Either a b) (b -> Either a b) String c SwmVotingContract (Either a b) 
ballotPropHelper = runFn5 getBallotPropImpl Left Right
foreign import getBallotPropAsyncImpl :: forall e b c. Fn3 String c SwmVotingContract (EffFnAff (| e) b) 
ballotPropHelperAff = runFn3 getBallotPropAsyncImpl

foreign import submitBallotImpl :: forall e. Fn4 Int Uint8Array Uint8Array SwmVotingContract (EffFnAff (| e) String)


-- contract setup functions
makeSwmVotingContract :: forall a. String -> Maybe SwmVotingContract
makeSwmVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

setWeb3Provider :: forall a. String -> a 
setWeb3Provider = runFn1 setWeb3ProviderImpl

-- web3 functions
getAccount :: Int -> Either String String
getAccount = runFn3 getAccountImpl Left Right 

-- contract functions
getBallotSK :: SwmVotingContract -> Either String Uint8Array
getBallotSK contract = do 
    skStr <- ballotPropHelper "ballotEncryptionSeckey" noArgs contract
    if skStr == "0x0000000000000000000000000000000000000000000000000000000000000000" then
            Left "Ballot Encryption Secret Key has not yet been published"
        else 
            fromEthHexE skStr

getBallotEndTime :: forall a. SwmVotingContract -> Either String String
getBallotEndTime = ballotPropHelper "endTime" noArgs


getBallotEncPK :: SwmVotingContract -> Either String String
getBallotEncPK = ballotPropHelper "ballotEncryptionPubkey" noArgs


setBallotEndTime :: forall e. Int -> SwmVotingContract -> Aff (| e) String
setBallotEndTime endTime contract = fromEffFnAff $ ballotPropHelperAff "setEndTime" [endTime] contract


releaseSecretKey :: forall e. String -> SwmVotingContract -> Aff (| e) String
releaseSecretKey secKey contract = fromEffFnAff $ ballotPropHelperAff "revealSeckey" ["0x" <> secKey] contract


web3CastBallot :: forall e. Int -> Tuple Uint8Array Uint8Array -> SwmVotingContract -> (Aff (| e) (String))
web3CastBallot accN (Tuple encBallot senderPk) contract = fromEffFnAff $ runFn4 submitBallotImpl accN encBallot senderPk contract


runBallotCount :: forall e. Maybe SwmVotingContract -> BallotResult
runBallotCount Nothing = Left "Contract is not initialized."
runBallotCount (Just contract) = 
    do  -- do in Either monad
        -- check time of ballot
        let nowTime = unsafePerformEff currentTimestamp
        endTimeStr <- getBallotEndTime contract
        endTime <- mToE "Could not convert endTime to Number" $ fromString endTimeStr
        let _ = unsafePerformEff $ log $ "Ballot end time: " <> (toString endTime) <> "\nCurrent Time:    " <> (toString nowTime)
        cont <- canContinue (nowTime > endTime) "The ballot has not ended yet!"
        
        -- get the secret key
        ballotSecKey <- getBallotSK contract

        -- get number of votes and then the votes
        nVotes <- (fromMaybe 0 <<< fromStringAs decimal) <$> (ballotPropHelper "nVotesCast" noArgs contract)
        ballots <- getBallots contract nVotes
        decryptedBallots <- maybe (Left "Ballots failed decryption") Right $ decryptBallots ballotSecKey ballots 

        pure ""
    where
        canContinue cond errMsg = if cond then Right true else Left errMsg


getBallots :: SwmVotingContract -> Int -> Either String (Array EncBallot)
getBallots _ 0 = Right []
getBallots contract n = do
    let currVote = n-1
    encBallot <- fromEthHexE =<< ballotPropHelper "encryptedBallots" [currVote] contract
    pubkey <- fromEthHexE =<< ballotPropHelper "associatedPubkeys" [currVote] contract
    address <- ballotPropHelper "associatedAddresses" [currVote] contract
    otherVotes <- getBallots contract currVote
    Right $ {encBallot, pubkey, address} : otherVotes


decryptBallots :: Uint8Array -> Array EncBallot -> Maybe (Array Ballot)
decryptBallots _ [] = Just []
decryptBallots seckey ballots = do
    {encBallot, pubkey, address} <- head ballots
    ballot <- toUint8Array <$> decryptOneTimeBallot (toBox encBallot) (toBoxPubkey pubkey) (toBoxSeckey seckey)
    remBallots <- tail ballots
    otherBallots <- decryptBallots seckey remBallots
    pure $ {ballot, pubkey, address} : otherBallots










