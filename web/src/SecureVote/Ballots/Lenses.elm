module SecureVote.Ballots.Lenses exposing (..)

import Monocle.Common exposing ((=>), maybe)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import SecureVote.Ballots.Types exposing (..)


mapBSpecWDefault : (BSpec01Impl -> a) -> a -> BallotSpec -> a
mapBSpecWDefault onV01 default b =
    case b of
        BVer01 b_ ->
            onV01 b_

        _ ->
            default


bTitle : Optional BallotSpec String
bTitle =
    Optional
        (mapBSpecWDefault (Just << .ballotTitle) Nothing)
        (\t bS -> mapBSpecWDefault (\b -> BVer01 { b | ballotTitle = t }) bS bS)


bStartTime : Optional BallotSpec Int
bStartTime =
    let
        g b =
            mapBSpecWDefault (Just << .startTime) Nothing b

        s a b =
            mapBSpecWDefault (\b_ -> BVer01 { b_ | startTime = a }) b b
    in
    Optional g s


bEndTime : Optional BallotSpec Int
bEndTime =
    Optional
        (mapBSpecWDefault (Just << .endTime) Nothing)
        (\a b -> mapBSpecWDefault (\b_ -> BVer01 { b_ | endTime = a }) b b)


bShortDesc : Optional BallotSpec String
bShortDesc =
    let
        g b =
            mapBSpecWDefault (Just << .shortDesc) Nothing b

        s a b =
            mapBSpecWDefault (\b_ -> BVer01 { b_ | shortDesc = a }) b b
    in
    Optional g s


bErc20Addr : Optional BallotSpec String
bErc20Addr =
    let
        g b =
            mapBSpecWDefault (Just << .erc20Addr) Nothing b

        s a b =
            mapBSpecWDefault (\b_ -> BVer01 { b_ | erc20Addr = a }) b b
    in
    Optional g s


bVoteOpts : Optional BallotSpec OptsOuter
bVoteOpts =
    Optional
        (mapBSpecWDefault (Just << .options) Nothing)
        (\a bSpec -> mapBSpecWDefault (\b -> BVer01 { b | options = a }) bSpec bSpec)


bDiscLink : Optional BallotSpec String
bDiscLink =
    Optional
        (mapBSpecWDefault .discussionLink Nothing)
        (\a bSpec -> mapBSpecWDefault (\b -> BVer01 { b | discussionLink = Just a }) bSpec bSpec)


bEncPK : Optional BallotSpec String
bEncPK =
    Optional
        (mapBSpecWDefault .encryptionPK Nothing)
        (\pk b -> mapBSpecWDefault (\b_ -> BVer01 { b_ | encryptionPK = Just pk }) b b)


optsToList : OptsOuter -> List SimpleOption
optsToList os =
    case os of
        OptsSimple RangeVotingPlusMinus3 vos ->
            vos

        OptsBinary ->
            [ { optionTitle = "Resolution", optionDesc = Nothing } ]

        OptsNothing ->
            []
