module SecureVote.Eth.Types exposing (..)

import Decimal exposing (Decimal)
import Json.Encode exposing (Value)


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
    { democHash : String, i : Int, specHash : String, extraData : String, votingContract : String, startTime : Int }


type alias WriteViaMMDoc =
    { abi : String, addr : String, method : String, args : Value }


type alias Erc20Abrv =
    { erc20Addr : String, abrv : String }
