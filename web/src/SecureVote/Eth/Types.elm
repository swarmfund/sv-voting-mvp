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


type alias ReadContractDoc =
    { abi : String, addr : String, method : String, args : Value }


type alias ReadResponse =
    { success : Bool, response : Value, errMsg : String, method : String, addr : String }


type alias Erc20Abrv =
    { erc20Addr : String, abrv : String, bHash : String }


type alias GotTxInfoPayload =
    { to : String, data : String }


type alias GetTxInfoContractWrite =
    { to : String, abi : String, method : String, args : Value }


type alias CandidateEthTx =
    { from : Maybe String
    , to : Maybe String
    , value : Int
    , data : Maybe String
    , gas : String
    }


nullCandidateEthTx : CandidateEthTx
nullCandidateEthTx =
    -- Gas set to 200,000
    { from = Nothing, to = Nothing, value = 0, data = Just "", gas = "0x030d40" }


type alias MinEthTx =
    { from : String
    , to : String
    , value : Int
    , data : String
    , gas : String
    }


type alias TxidLog =
    { namespace : String
    , txid : String
    , extra : Value
    }


zeroAddr =
    "0x0000000000000000000000000000000000000000"
