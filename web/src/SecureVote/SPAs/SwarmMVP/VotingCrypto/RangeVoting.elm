module SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (..)

import Dict exposing (Dict)
import Maybe.Extra exposing ((?))
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.Types exposing (..)
import SecureVote.Eth.Types exposing (EthAddress)
import SecureVote.Eth.Utils exposing (fromHexEth)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (genVoteOptId)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.Types.VBit exposing (vBitsToBytes, vblToList)
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits)


byteCheck : Int -> Bool
byteCheck b =
    b >= 0 && b <= 255


type alias RangeBallotPlaintext =
    -- List of 16 bytes
    List Int


rangeBallotTxtCheck : RangeBallotPlaintext -> Bool
rangeBallotTxtCheck ballotBytes =
    let
        valCheck =
            List.all byteCheck ballotBytes

        lenCheck =
            List.length ballotBytes == 16
    in
    valCheck && lenCheck


constructBallot : List RangeBallot3Bits -> EthAddress -> Maybe RangeBallotPlaintext
constructBallot votes delegate =
    let
        voteBytes =
            vBitsToBytes 2 <| List.concat (List.map vblToList votes)

        ethPrefixM =
            Maybe.map (List.take 14) <| fromHexEth delegate
    in
    Maybe.map (\prefix -> voteBytes ++ prefix) ethPrefixM


orderedBallotBits : ( String, BallotSpec ) -> Dict Int (Result String RangeBallot3Bits) -> Maybe (List RangeBallot3Bits)
orderedBallotBits ( bHash, bSpec ) ballotBitsDict =
    let
        ballotIds =
            List.map (genVoteOptId bHash) (List.range 0 <| optsNOptions <| bVoteOpts.getOption bSpec ? OptsNothing)

        ballotBits =
            Maybe.Extra.combine <| List.map (\id -> Dict.get id ballotBitsDict ? Err "" |> Result.toMaybe) ballotIds
    in
    ballotBits
