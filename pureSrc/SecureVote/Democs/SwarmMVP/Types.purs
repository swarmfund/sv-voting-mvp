module SecureVote.Democs.SwarmMVP.Types
    ( SwmBallot()
    , Address()
    , Delegate()
    , Vote()
    , Votes()
    , Voter()
    , VotesRecord
    , delegateToAddr
    , addrToString
    , delegateToString
    , voterToAddr
    , voterToString
    , toSwmBallot
    , toDelegate
    , toAddress
    , toVoter
    , getVoter
    , unsafeToAddress
    , getVotes
    , voteToInt
    , swmBallotShowJustVotes
    ) where



import Prelude

import Data.Array (foldl, length, take)
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), maybe)
import Data.String (toLower)
import Data.Traversable (sequence)
import SecureVote.Utils.Numbers (intToStr)



data SwmBallot = SwmBallot Voter Vote Vote Vote Vote
instance showSwmBallot :: Show SwmBallot where
    show (SwmBallot v v1 v2 v3 v4) = 
        "[ SwmBallot: " <>
        "Vtr: " <> show v <> ", " <> 
        "Vs: " <> foldl (\b a -> b <> " " <> show a) (show v1) [v2, v3, v4] <> " ]"


swmBallotShowJustVotes :: SwmBallot -> String
swmBallotShowJustVotes (SwmBallot v v1 v2 v3 v4) = show [v1, v2, v3, v4]


newtype Address = Address String
instance showAddr :: Show Address where 
    show = addrToString
derive instance ordAddr :: Ord Address
derive instance eqAddr :: Eq Address

newtype Vote = Vote Int
instance showVote :: Show Vote where 
    show (Vote i) = toStringAs decimal i

newtype Votes = Votes (Array Vote)
instance showVotes :: Show Votes where 
    show (Votes a) = show a 

newtype Delegate = Delegate (Address)
instance showDelegate :: Show Delegate where 
    show = delegateToString
derive instance ordDelegate :: Ord Delegate
derive instance eqDelegate :: Eq Delegate

newtype Voter = Voter (Address)
instance showVoter :: Show Voter where 
    show = voterToString
derive instance ordVoter :: Ord Voter
derive instance eqVoter :: Eq Voter


type VotesRecord = {v1::Vote, v2::Vote, v3::Vote, v4::Vote}


-- utility functions
foreign import checkAddressImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe String)


-- utility functions
checkAddress :: forall a. String -> Maybe String
checkAddress = runFn3 checkAddressImpl Just Nothing



delegateToAddr :: Delegate -> Address
delegateToAddr (Delegate addr) = addr

addrToString :: Address -> String 
addrToString (Address str) = str

delegateToString :: Delegate -> String
delegateToString = addrToString <<< delegateToAddr

voterToAddr :: Voter -> Address
voterToAddr (Voter addr) = addr

voterToString :: Voter -> String
voterToString = addrToString <<< voterToAddr


toSwmBallot :: Voter -> Array Int -> Either String SwmBallot
toSwmBallot voter votes = do
        checkedVotes <- toVotes votes
        {v1, v2, v3, v4} <- destructureVotes checkedVotes
        pure $ SwmBallot voter v1 v2 v3 v4


getVoter :: SwmBallot -> Voter
getVoter (SwmBallot voter _ _ _ _) = voter

getVotes :: SwmBallot -> VotesRecord
getVotes (SwmBallot _ v1 v2 v3 v4) = {v1, v2, v3, v4}
        

toVotes :: Array Int -> Either String Votes
toVotes unsVotes = do
        vs <- sequence $ checkRange <$> take 4 unsVotes
        if length vs /= 4 then Left $ "Ballot was incorrect length: " <> show vs
                          else pure $ Votes $ map Vote vs
    where
        -- all ballots for swarm must be in the range [-3, 3]
        checkRange v = if v > 3 || v < -3 then Left $ "Vote scalar out of range: " <> (intToStr v) else Right v


voteToInt :: Vote -> Int
voteToInt (Vote v) = v 


toAddress :: String -> Maybe Address
toAddress addr = Address <$> toLower <$> checkAddress addr


toVoter :: Address -> Voter
toVoter addr = Voter addr


toDelegate :: Address -> Delegate
toDelegate addr = Delegate addr


destructureVotes :: Votes -> Either String VotesRecord
destructureVotes (Votes [v1, v2, v3, v4]) = Right {v1, v2, v3, v4}
destructureVotes vs = Left $ "Votes was not of length 4: " <> show vs


unsafeToAddress :: String -> Address
unsafeToAddress s = Address $ toLower s