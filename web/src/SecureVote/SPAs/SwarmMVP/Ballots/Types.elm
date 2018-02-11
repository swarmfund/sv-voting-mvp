module SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (..)

import Decimal exposing (Decimal)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml)


type alias BallotParams msg =
    { voteOptions : List (BallotOption msg)
    , openingDesc : String
    , description : String
    , ballotTitle : String
    , contractAddr : String
    , startTime : Int
    , endTime : Int
    , erc20Addr : String
    , erc20Balance : Maybe Decimal
    , erc20Abrv : String -- Short abbreviation for token name
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
