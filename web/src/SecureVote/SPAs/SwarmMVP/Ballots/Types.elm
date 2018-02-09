module SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (..)

import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml)


type alias BallotParams msg =
    { voteOptions : List (BallotOption msg)
    , openingDesc : String
    , description : String
    , ballotTitle : String
    , contractAddr : String
    , startTime : Int
    , endTime : Int
    , id : Int -- Just some random integer
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
