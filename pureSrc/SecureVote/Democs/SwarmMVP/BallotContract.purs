module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5, runFn6)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Math (round)
import SecureVote.Utils.Time (currentTimestamp)


data SwmVotingContract = SwmVotingContract SwmVotingContract


type BallotResult = (Either String String)


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
getBallotSK :: forall a. SwmVotingContract -> Maybe String
getBallotSK = runFn3 getBallotSKImpl Just Nothing


getBallotEndTime :: forall a. SwmVotingContract -> Either String Number
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
    do
        -- check time of ballot
        let nowTime = unsafePerformEff currentTimestamp
        endTime <- getBallotEndTime contract
        let _ = unsafePerformEff $ log $ "Ballot end time: " <> (toString endTime) <> "\nCurrent Time:    " <> (toString nowTime)
        cont <- canContinue (nowTime > endTime) "The ballot has not ended yet!"
        -- get the secret key
        ballotSecKey <- ballotSkE
        pure $ ballotSecKey
    where
        canContinue cond errMsg = if cond then Right true else Left errMsg
        ballotSkM = getBallotSK contract
        ballotSkE = maybe (Left "Contract returned nothing for secret key - has it been published yet?") Right ballotSkM







