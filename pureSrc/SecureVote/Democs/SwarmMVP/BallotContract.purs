module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, runFn0, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromMaybe, maybe)


data SwmVotingContract = SwmVotingContract SwmVotingContract


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import setWeb3ProviderImpl :: forall a. Fn1 String a

-- contract FFI
foreign import getBallotSKImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) SwmVotingContract (Maybe String)


-- contract setup functions
makeSwmVotingContract :: forall a. String -> Maybe SwmVotingContract
makeSwmVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

setWeb3Provider :: forall a. String -> a 
setWeb3Provider = runFn1 setWeb3ProviderImpl


-- contract functions
getBallotSK :: forall a. SwmVotingContract -> Maybe String
getBallotSK = runFn3 getBallotSKImpl Just Nothing


runBallotCount :: Maybe SwmVotingContract -> Either String String 
runBallotCount Nothing = Left "Contract failed to intitialise."
runBallotCount (Just contract) = 
    do
        ballotSecKey <- ballotSkE
        pure ballotSecKey
    where
      ballotSkM = getBallotSK contract
      ballotSkE = maybe (Left "Contract returned nothing for secret key") Right ballotSkM







