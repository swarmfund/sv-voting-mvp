module SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (..)

import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml)


type alias BallotParams msg =
    { voteOptions : List (BallotOption msg)
    , openingDesc : String
    , ballotTitle : String
    , contractAddr : String
    , startTime : Int
    , endTime : Int
    }


type alias BallotOption msg =
    { id : Int
    , title : String
    , description : DialogHtml msg
    }


type alias ReleaseSchedule =
    { nReleases : Int
    , releaseLength : Int
    }
