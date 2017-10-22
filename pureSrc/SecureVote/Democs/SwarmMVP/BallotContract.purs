module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Time.Duration (Milliseconds(..))
import Math (round)
import SecureVote.Utils.Time (currentTimestamp)


data SwmVotingContract = SwmVotingContract SwmVotingContract


noArgs :: Unit
noArgs = unit


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import setWeb3ProviderImpl :: forall a. Fn1 String a

-- contract FFI
foreign import getBallotSKImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) SwmVotingContract (Maybe a)
foreign import getBallotPropImpl :: forall a b c. Fn5 (a -> Either a b) (b -> Either a b) String c SwmVotingContract (Either a b) 
ballotPropHelper = runFn5 getBallotPropImpl Left Right


-- contract setup functions
makeSwmVotingContract :: forall a. String -> Maybe SwmVotingContract
makeSwmVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

setWeb3Provider :: forall a. String -> a 
setWeb3Provider = runFn1 setWeb3ProviderImpl


-- contract functions
getBallotSK :: forall a. SwmVotingContract -> Maybe String
getBallotSK = runFn3 getBallotSKImpl Just Nothing


getBallotEndTime :: forall a. SwmVotingContract -> Either String Number
getBallotEndTime = ballotPropHelper "endTime" noArgs


getBallotEncPK :: SwmVotingContract -> Either String String
getBallotEncPK = ballotPropHelper "ballotEncryptionPubkey" noArgs


runBallotCount :: forall e. Maybe SwmVotingContract -> (Either String String)
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







