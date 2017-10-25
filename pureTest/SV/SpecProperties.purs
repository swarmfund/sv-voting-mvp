module Test.SV.SpecProperties where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Int (binary, fromStringAs, round)
import Data.Map (fromFoldable, lookup, Map)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord (abs)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.TypedArray (asArray, asUint8Array)
import Node.Yargs.Applicative (yarg)
import SecureVote.Democs.SwarmMVP.BallotContract (noArgs, removeDelegationLoops, removeHangingDelegations)
import SecureVote.Utils.ArrayBuffer (UI8AShowable(..), fromHex, toHex)
import SecureVote.Utils.Numbers (intByteToBitStr)
import Test.SV.Types (SpecType)
import Test.Spec (it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


startingDelegateMap :: Map Int (Maybe Int)
startingDelegateMap = fromFoldable 
                [ Tuple 1 (Just 2)
                , Tuple 2 (Nothing)
                , Tuple 3 (Just 4)
                , Tuple 4 (Just 5)
                , Tuple 5 (Just 6)
                , Tuple 6 (Just 4)
                , Tuple 7 (Just 8)
                , Tuple 8 (Just 9)
                , Tuple 9 (Just 10)
                , Tuple 10 (Just 9)
                , Tuple 11 (Just 11)
                , Tuple 12 (Just 9999)
                , Tuple 13 (Just 34737)
                ]


hasB = Right 1
noB = Left ""

pretendBallotMap :: Map Int (Either String Int)
pretendBallotMap = fromFoldable
                [ Tuple 1 hasB
                , Tuple 2 hasB
                , Tuple 3 hasB
                , Tuple 4 noB
                , Tuple 5 noB
                , Tuple 6 hasB
                , Tuple 7 hasB
                , Tuple 8 noB
                , Tuple 9 hasB
                , Tuple 10 noB
                -- deliberately don't put all keys in this map
                ]


specProperties :: forall e. SpecType e
specProperties = do
    it "should remove hanging delegations" do
        y 1
        n 2
        n 3
        n 4
        y 5  -- even though 5 has no ballot, his entry in the delegate map should 
             -- still point to 6 because 6 DID submit a ballot
        n 6
        n 7
        y 8
        n 9
        y 10
        n 11
        n 12
        n 13

    it "should remove delegation loops BUT NOT delegations leading into the loop that aren't part of it" do
        checkMatch 1 2
        checkNothing 2
        checkMatch 3 4
        checkNothing 4
        checkNothing 5
        checkNothing 6
        checkMatch 7 8
        checkMatch 8 9
        checkNothing 9
        checkNothing 10
        checkNothing 11

    where
        m = startingDelegateMap
        mNoLoops = removeDelegationLoops m
        mNoHanging = removeHangingDelegations m pretendBallotMap
        y a = (isJust $ join $ lookup a mNoHanging) `shouldEqual` true
        n a = (isJust $ join $ lookup a mNoHanging) `shouldEqual` false
        checkMatch a b = (join $ lookup a mNoLoops) `shouldEqual` (Just b)
        checkNothing a = Nothing `shouldEqual` (join $ lookup a mNoLoops)
