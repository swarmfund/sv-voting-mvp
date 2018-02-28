module SecureVote.Eth.Types exposing (..)

import Decimal exposing (Decimal)


type alias EthAddress =
    String


type alias InitRecord =
    { miniAbi : String }


type AuditDoc
    = AuditLog String
    | AuditLogErr String
    | AuditFail String
    | AuditSuccess BallotResult


type alias BallotResult =
    { nVotes : Int, totals : List ( String, Decimal ) }


type alias BallotInfo =
    { democHash : String, i : Int, specHash : String, extraData : String, votingContract : String }
