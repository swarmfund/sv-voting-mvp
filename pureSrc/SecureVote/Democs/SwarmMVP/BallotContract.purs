module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Monad.Aff (Aff, error, launchAff, parallel, sequential, throwError)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxPublicKey, BoxSecretKey, toUint8Array)
import Data.Array (head, range, tail, (:))
import Data.ArrayBuffer.Types (Uint8Array)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Either (Either(..), either)
import Data.Foreign.Keys (keys)
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5, runFn6)
import Data.Int (decimal, fromStringAs, toNumber, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Math (round)
import SecureVote.Crypto.Curve25519 (decryptOneTimeBallot, toBox, toBoxPubkey, toBoxSeckey)
import SecureVote.Utils.ArrayBuffer (fromEthHex, fromEthHexE, fromHex, fromHexE, toHex)
import SecureVote.Utils.Monads (mToE)
import SecureVote.Utils.Poly (self)
import SecureVote.Utils.Time (currentTimestamp)
import Unsafe.Coerce (unsafeCoerce)


data SwmVotingContract = SwmVotingContract SwmVotingContract


type BallotResult = String


type Ballot = {ballot :: Uint8Array, voterPk :: Uint8Array, voterAddr :: String}
type EncBallot = {encBallot :: Uint8Array, voterPk :: Uint8Array, voterAddr :: String}
type EncBallotWVoter = {encBallot :: Uint8Array, voterPk :: BoxPublicKey}


noArgs :: forall a. Array a
noArgs = []


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import setWeb3ProviderImpl :: forall a. Fn1 String a

-- web3 FFI
foreign import getAccountImpl :: forall a b. Fn3 (a -> Either a b) (b -> Either a b) Int (Either String String)

-- contract FFI
foreign import getBallotSKImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) SwmVotingContract (Maybe a)
foreign import getBallotPropImpl :: forall a b c. Fn5 (a -> Either a b) (b -> Either a b) String c SwmVotingContract (Either a b) 
ballotPropHelper = runFn5 getBallotPropImpl Left Right
foreign import getBallotPropAsyncImpl :: forall e b c. Fn3 String (Array c) SwmVotingContract (EffFnAff (| e) b) 

ballotPropHelperEffFnAff :: forall a b eff. String -> Array a -> SwmVotingContract -> EffFnAff (| eff) b
ballotPropHelperEffFnAff = runFn3 getBallotPropAsyncImpl

ballotPropHelperAff :: forall a b eff. String -> (Array a) -> SwmVotingContract -> (Aff (| eff) b)
ballotPropHelperAff prop args contract = fromEffFnAff (runFn3 getBallotPropAsyncImpl prop args contract)

foreign import submitBallotImpl :: forall e. Fn4 Int Uint8Array BoxPublicKey SwmVotingContract (EffFnAff (| e) String)


-- contract setup functions
makeSwmVotingContract :: forall a. String -> Maybe SwmVotingContract
makeSwmVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

setWeb3Provider :: forall a. String -> a 
setWeb3Provider = runFn1 setWeb3ProviderImpl

-- web3 functions
getAccount :: Int -> Either String String
getAccount = runFn3 getAccountImpl Left Right 

-- contract functions
swmBallotSk :: forall e. SwmVotingContract -> Aff (| e) (BoxSecretKey)
swmBallotSk contract = do 
    skStr <- ballotPropHelperAff "ballotEncryptionSeckey" noArgs contract
    if skStr == "0x0000000000000000000000000000000000000000000000000000000000000000" then
            throwError $ error "Ballot Encryption Secret Key has not yet been published"
        else either (throwError <<< error) (pure <<< toBoxSeckey) (fromEthHexE skStr)


getBallotEndTime :: forall a. SwmVotingContract -> Either String String
getBallotEndTime = ballotPropHelper "endTime" noArgs


getBallotEncPK :: SwmVotingContract -> Either String String
getBallotEncPK = ballotPropHelper "ballotEncryptionPubkey" noArgs


setBallotEndTime :: forall e. Int -> SwmVotingContract -> Aff (| e) String
setBallotEndTime endTime contract = fromEffFnAff $ ballotPropHelperEffFnAff "setEndTime" [endTime] contract


releaseSecretKey :: forall e. String -> SwmVotingContract -> Aff (| e) String
releaseSecretKey secKey contract = fromEffFnAff $ ballotPropHelperEffFnAff "revealSeckey" ["0x" <> secKey] contract


web3CastBallot :: forall e. Int -> EncBallotWVoter -> SwmVotingContract -> (Aff (| e) (String))
web3CastBallot accN {encBallot, voterPk} contract = fromEffFnAff $ runFn4 submitBallotImpl accN encBallot voterPk contract


swmEndTime :: forall e. SwmVotingContract -> Aff (| e) Number
swmEndTime contract = do
    endTimeStr <- ballotPropHelperAff "endTime" noArgs contract
    let endTimeM = fromString endTimeStr
    maybe (throwError $ error $ "endTime could not be converted to a Number: " <> endTimeStr) pure endTimeM


swmNVotes :: forall e. SwmVotingContract -> Aff (| e) Int
swmNVotes contract = do
    nVotesStr <- ballotPropHelperAff "nVotesCast" noArgs contract
    let nVotes = fromStringAs decimal nVotesStr
    maybe (throwError $ error $ "nVotes was not an integer: " <> nVotesStr) pure nVotes


runBallotCount :: forall e. Maybe SwmVotingContract -> (Aff (now :: NOW, console :: CONSOLE | e) (Either String BallotResult))
runBallotCount Nothing = pure $ Left "Contract is not initialized."
runBallotCount (Just contract) = 
    do  -- Aff monad
        nowTime <- liftEff $ currentTimestamp
        endTime <- swmEndTime contract 
        log $ "Ballot end time: " <> (toString endTime) <> "\nCurrent Time:    " <> (toString nowTime)
        if nowTime < endTime 
            then pure $ Left "The ballot has not ended yet!"
            else do
                ballotSeckey <- swmBallotSk contract
                nVotes <- swmNVotes contract 
                encBallots <- getBallotsAff contract nVotes
                decryptedBallots <- decryptBallots ballotSeckey encBallots

                pure $ Right "done"


getBallotsAff :: forall e. SwmVotingContract -> Int -> Aff (| e) (Array EncBallot)
getBallotsAff contract n 
    | n <= 0 = pure []
    | otherwise = do
        let allVoteIds = range 0 (n-1)
        parTraverse getBallot allVoteIds
    where
        getBallot i = do
            encBallot <- keyFromEthHex $ ballotPropHelperAff "encryptedBallots" [i] contract
            voterPk <- keyFromEthHex $ ballotPropHelperAff "associatedPubkeys" [i] contract
            voterAddr <- ballotPropHelperAff "associatedAddresses" [i] contract
            pure $ {encBallot, voterPk, voterAddr}
        keyFromEthHex k = do
            ethHex <- k
            either (throwError <<< error) pure (fromEthHexE ethHex)


decryptBallots :: forall e. BoxSecretKey -> Array EncBallot -> Aff (| e) (Array Ballot)
decryptBallots _ [] = pure []
decryptBallots encSk ballots = do
        parTraverse decryptOne ballots
    where
        decryptOne {encBallot, voterPk, voterAddr} = do
            let ballotM = toUint8Array <$> (decryptOneTimeBallot (toBox encBallot) (toBoxPubkey voterPk) encSk)
            maybe (throwError $ error $ "Unable to decrypt ballot from: " <> voterAddr) (\ballot -> pure $ {ballot, voterPk, voterAddr}) ballotM










