module SecureVote.SPAs.SwarmMVP.Ballot exposing (allBallots, doBallotOptsMatch)

import SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (rschedBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams)


doBallotOptsMatch : List (BallotOption msg) -> List String -> Bool
doBallotOptsMatch voteOpts titlesFromEth =
    True


allBallots : List (BallotParams msg)
allBallots =
    [ rschedBallot
    ]
