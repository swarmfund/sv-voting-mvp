module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)

import Debug exposing (log)
import Maybe.Extra exposing (combine)
import SecureVote.Eth.Utils exposing (keccak256OverString, toHex)
import SecureVote.SPAs.SwarmMVP.Ballots.DevBasicBallot exposing (allDevBallots_, devBasicBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (rschedBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams)


emptyStringHash : String
emptyStringHash =
    "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"


{-| We get the Keccak256 hash back from Ethereum |
-}
doBallotOptsMatch : List (BallotOption msg) -> List String -> Bool
doBallotOptsMatch voteOpts titlesFromEth =
    let
        voteTitles =
            List.map (\{ title } -> title) voteOpts

        voteHashes =
            Maybe.map (List.filter (\h -> h /= emptyStringHash)) <| combine <| List.map keccak256OverString voteTitles

        ethHashes =
            List.filter (\h -> h /= emptyStringHash) titlesFromEth

        ans =
            case voteHashes of
                Nothing ->
                    False

                Just vhs ->
                    vhs == ethHashes

        _ =
            log ("voteTitles: " ++ toString voteTitles) ""

        _ =
            log ("voteHashes: " ++ toString voteHashes) ""

        _ =
            log ("ethHashes: " ++ toString ethHashes) ""
    in
    ans


allBallots : List (BallotParams msg)
allBallots =
    [ rschedBallot
    ]


allDevBallots : List (BallotParams msg)
allDevBallots =
    allDevBallots_


initBallot : BallotParams msg
initBallot =
    rschedBallot


initDevBallot : BallotParams msg
initDevBallot =
    devBasicBallot
