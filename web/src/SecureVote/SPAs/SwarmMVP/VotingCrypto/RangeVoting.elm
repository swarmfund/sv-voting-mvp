module SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (..)

import SecureVote.Eth.Types exposing (EthAddress)
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits)


type alias Nonce =
    -- List of 6 bytes
    List Int


nonceCheck : Nonce -> Bool
nonceCheck nonce =
    True


type alias RangeBallotPlaintext =
    -- List of 16 bytes
    List Int


rangeBallotTxtCheck : RangeBallotPlaintext -> Bool
rangeBallotTxtCheck ballotBytes =
    let
        byteRangeCheck =
            List.all <| List.map (\b -> b >= 0 && b <= 255) ballotBytes

        lengthCheck =
            List.length ballotBytes == 16
    in
    byteRangeCheck && lengthCheck


constructBallot : Nonce -> List RangeBallot3Bits -> EthAddress -> RangeBallotPlaintext
