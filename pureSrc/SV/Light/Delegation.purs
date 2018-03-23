module SV.Light.Delegation where


import SV.Prelude

import Control.Monad.Aff.Console as AffC
import Control.Parallel (parTraverse)
import Data.Array (foldr)
import Data.Array as Arr
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (sequence)
import Network.Ethereum.Web3 (Address, ChainCursor, mkAddress, mkHexString)
import Network.Ethereum.Web3.Solidity (Tuple2(..), Tuple6(..))
import Partial.Unsafe (unsafePartial)
import SV.Light.Types.BallotBox (BallotFromSC)
import SV.Light.Types.RunBallot (SmartContract)
import SecureVote.Contracts.SVDelegationV0101 (findPossibleDelegatorsOf, resolveDelegation)
import SecureVote.Utils.Array (onlyJust)


mainnetDlgtAddr :: Address
mainnetDlgtAddr = unsafePartial fromJust $ mkAddress =<< mkHexString "0x4dD28be042F85e287E9AaCe4147152bf1CD835e9"

kovanDlgtAddr :: Address
kovanDlgtAddr = unsafePartial fromJust $ mkAddress =<< mkHexString "0x8F6F18b9A83E0b42cE69783a8282441BF8F417fc"


dlgtAddr :: {dev :: Boolean} -> Address
dlgtAddr {dev} = if dev then kovanDlgtAddr else mainnetDlgtAddr


procResDlgtion :: (Tuple6 _ _ _ Address Address Address) -> {delegator :: Address, delegatee :: Address, token :: Address}
procResDlgtion (Tuple6 _ _ _ delegatee delegator token) = {delegatee, delegator, token}


-- | Find all delegators to a particular delegate recursively
findDelegatorsRecursive :: {tknAddr :: Address, delegate :: Address} -> (forall args r. SmartContract _ args r) -> ChainCursor -> Aff _ (Array Address)
findDelegatorsRecursive = findDelegators_ Set.empty
  where
    findDelegators_ :: Set.Set Address -> _ -> (forall args r. SmartContract _ args r) -> _ -> Aff _ (Array Address)
    findDelegators_ carry opts@{tknAddr, delegate} dlgtSC cc = do
        Tuple2 delegators tokens <- dlgtSC findPossibleDelegatorsOf cc {delegate}
        AffC.log $ show (Arr.length delegators) <> " possible delegators to " <> show delegate
        let dlgtPairs = Arr.filter (\(Tuple vtr tkn) -> not (Set.member vtr carry)) $ Arr.zip delegators tokens
        AffC.log $ "Finding delegators for " <> show delegate <> ", dlgtPairs len: " <> show (Arr.length dlgtPairs)
        realDelegators <- onlyJust <$> parTraverse confirmDelegation dlgtPairs
        let checkNext = fst <$> realDelegators
            nextCarry = carry <> Set.fromFoldable checkNext
        AffC.log $ "Finding delegators recursively for " <> show delegate <> ", nextCarry n: " <> show (Set.size nextCarry)
        deeperDelegators <- foldr (<>) [] <$> parTraverse (\newDlgt -> findDelegators_ nextCarry (opts {delegate = newDlgt}) dlgtSC cc) checkNext
        AffC.log $ "Got delegators for " <> show delegate <> ", n: " <> show (Arr.length deeperDelegators)
        pure $ Set.toUnfoldable (nextCarry <> Set.fromFoldable deeperDelegators)
      where
        confirmDelegation p@(Tuple vtr tkn) = do
            Tuple6 _ _ _ delegatee delegator token <- dlgtSC resolveDelegation cc {voter: vtr, tokenContract: tkn}
            if delegatee == delegate then pure (Just p) else pure Nothing


-- | returns a map of all relevant (based on voters) delegations where each delegator points to their delegatee.
-- | the idea is to map over all relevant balances and check the delegate map for the _first_ vote made
-- | by a _delegator_ and add the balance to that vote.
getDelegates :: {tknAddr :: Address, allBallots :: Array (BallotFromSC)} -> (forall args r. SmartContract _ args r) -> ChainCursor -> Aff _ (Map Address Address)
getDelegates {tknAddr, allBallots} dlgtSC chainCursor = do
    let allVoters = (\{voterAddr} -> voterAddr) <$> allBallots
    (allBaseDelegators :: Set.Set Address) <- (foldr (<>) Set.empty) <$> parTraverse (\v -> Set.fromFoldable <$> findDelegatorsRecursive {tknAddr, delegate: v} dlgtSC chainCursor) allVoters
    Map.fromFoldable <$> parTraverse (\dlgtr -> (getDlgtPair <<< procResDlgtion) <$> dlgtSC resolveDelegation chainCursor {voter : dlgtr, tokenContract: tknAddr}) (Set.toUnfoldable allBaseDelegators :: Array _)
  where
    getDlgtPair {delegatee, delegator} = Tuple delegator delegatee
