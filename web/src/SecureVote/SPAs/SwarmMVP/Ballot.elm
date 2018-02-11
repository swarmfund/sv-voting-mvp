module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)

import SecureVote.SPAs.SwarmMVP.Ballots.DevBasicBallot exposing (devBasicBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (rschedBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams)


doBallotOptsMatch : List (BallotOption msg) -> List String -> Bool
doBallotOptsMatch voteOpts titlesFromEth =
    True


allBallots : List (BallotParams msg)
allBallots =
    [ rschedBallot
    ]


allDevBallots : List (BallotParams msg)
allDevBallots =
    [ devBasicBallot
    ]


initBallot : BallotParams msg
initBallot =
    rschedBallot


initDevBallot : BallotParams msg
initDevBallot =
    devBasicBallot
