port module SecureVote.Eth.Web3 exposing (..)

import Debug
import Decimal exposing (Decimal, fromString)
import Json.Decode as Decode exposing (Decoder, Value, bool, decodeValue, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (combine)
import SecureVote.Eth.Models exposing (CandidateEthTx, MinEthTx)
import SecureVote.Eth.Types exposing (AuditDoc(..), BallotResult, InitRecord)
import SecureVote.Eth.Utils exposing (dropEthPrefix)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))
import SecureVote.SPAs.SwarmMVP.Types exposing (GotTxidResp)


port gotMetamaskImpl : (Value -> msg) -> Sub msg


gotMetamask : Sub Msg
gotMetamask =
    gotMetamaskImpl
        (\val ->
            case Decode.decodeValue bool val of
                Ok _ ->
                    FromWeb3 GotMetaMask

                Err err ->
                    LogErr err
        )


port checkTxid : String -> Cmd msg


port gotTxidCheckStatus : (Value -> msg) -> Sub msg


onGotTxidStatus : Value -> Msg
onGotTxidStatus val =
    let
        decoder =
            decode GotTxidResp
                |> required "data" string
                |> required "confirmed" bool
                |> required "gas" int
                |> required "logMsg" string
    in
    case Decode.decodeValue decoder val of
        Ok resp ->
            FromWeb3 <| GotTxidStatus <| Ok resp

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GotTxidStatus (Err "Decoding Error") ]


type alias ConsDataParamReq =
    { encBallot : String, voterPubkey : String, votingContractAddr : String }


type alias PerformRead =
    { addr : String
    , method : String
    , args : Value
    }


port performContractRead : PerformRead -> Cmd msg


type alias ReadResponse =
    { success : Bool
    , response : Value
    , errMsg : String
    , method : String
    }


port contractReadResponse : (Value -> msg) -> Sub msg


onContractReadResponse : (ReadResponse -> msg) -> (String -> msg) -> Value -> msg
onContractReadResponse msgGen errMsg val =
    let
        _ =
            Debug.log ("contract read got back err: " ++ toString val) True

        decoder =
            decode ReadResponse
                |> required "success" Decode.bool
                |> required "response" Decode.value
                |> required "errMsg" Decode.string
                |> required "method" Decode.string
    in
    case Decode.decodeValue decoder val of
        Ok secVal ->
            if secVal.success then
                msgGen secVal
            else
                errMsg secVal.errMsg

        Err err ->
            errMsg err


type alias GetErc20BalanceReq =
    { contractAddress : String, userAddress : String }


port getErc20Balance : GetErc20BalanceReq -> Cmd msg


port implErc20Balance : (Value -> msg) -> Sub msg


onIncomingErc20Balance : Value -> Msg
onIncomingErc20Balance encodedBalance =
    let
        decResult =
            Decode.decodeValue string encodedBalance
    in
    case decResult |> Result.andThen (Decimal.fromString >> Result.fromMaybe "Decimal conversion failed") of
        Ok bal ->
            FromWeb3 <| GotBalance bal

        Err _ ->
            LogErr <| "Got bad balance back from Web3 " ++ toString encodedBalance


port setWeb3Provider : String -> Cmd msg


port gotWeb3Error : (Value -> msg) -> Sub msg


errHelper : String -> a -> Msg
errHelper descMsg errMsg =
    let
        errStr =
            toString errMsg
    in
    LogErr (Debug.log errStr <| descMsg ++ errStr)


onIncomingWeb3Error : Value -> Msg
onIncomingWeb3Error err =
    case Decode.decodeValue string err of
        Ok err_ ->
            errHelper "Got error back from Web3: " err_

        Err _ ->
            errHelper "Unable to decode Web3 error!!! Check console.log: " err


port constructDataParam : ConsDataParamReq -> Cmd msg


port castMetaMaskVoteImpl : MinEthTx -> Cmd msg


castMetaMaskVote : CandidateEthTx -> Cmd msg
castMetaMaskVote { from, to, value, data, gas } =
    castMetaMaskVoteImpl
        { from = withDefault "" from
        , to = withDefault "ERROR - THIS SHOULD BE FILLED AND YOU SHOULD NEVER SEE THIS" to
        , value = value
        , data = withDefault "ERROR - THIS SHOULD BE FILLED AND YOU SHOULD NEVER SEE THIS" data
        , gas = gas
        }


port metamaskTxidImpl : (Value -> msg) -> Sub msg


metamaskTxid : Sub Msg
metamaskTxid =
    metamaskTxidImpl <|
        \val ->
            case decodeValue string val of
                Ok txid ->
                    FromWeb3 <| GotMetaMaskTxid txid

                Err err ->
                    errHelper "MetaMask returned an error: " err


port implDataParam : (Value -> msg) -> Sub msg


onRecieveDataParam : Value -> Msg
onRecieveDataParam dataVal =
    case Decode.decodeValue string dataVal of
        Ok data ->
            FromWeb3 <| GotDataParam data

        Err err ->
            errHelper "onRecieveDataParam: Unable to decode data param from web3! Check console.log: " err


port getEncryptionPublicKey : String -> Cmd msg


port gotEncPubkey : (Value -> msg) -> Sub msg


onGotPubkey : Value -> Msg
onGotPubkey pubkeyVal =
    case Decode.decodeValue string pubkeyVal of
        Ok pubkey ->
            FromWeb3 <| GotEncPubkey <| dropEthPrefix pubkey

        Err err ->
            errHelper "Error while retrieving encryption public key: " err


port getInit : String -> Cmd msg


port implInit : (Value -> msg) -> Sub msg


onInit : (InitRecord -> Msg) -> Value -> Msg
onInit msgConstructor initStuff =
    let
        decoder =
            decode InitRecord
                |> required "miniAbi" string
    in
    case decodeValue decoder initStuff of
        Ok init ->
            msgConstructor init

        Err err ->
            errHelper "Error while getting initial parameters from Web3: " err


port getBallotResults : { ethUrl : String, ethRPCAuth : String, votingAddr : String, erc20Addr : String } -> Cmd msg


port gotAuditMsgImpl : (Value -> msg) -> Sub msg


decodeAuditMsg : Value -> Msg
decodeAuditMsg auditVal =
    let
        procSuccessKVs : Decoder (List ( String, String )) -> Decoder (List ( String, Decimal ))
        procSuccessKVs =
            Decode.andThen
                (\listOfStringRes ->
                    case
                        combine <|
                            List.map
                                (\( s, dStr ) ->
                                    case fromString dStr of
                                        Nothing ->
                                            Nothing

                                        Just d ->
                                            Just ( s, d )
                                )
                                listOfStringRes
                    of
                        Nothing ->
                            Decode.fail "Unable to decode BallotTotals from Auditor"

                        Just res ->
                            Decode.succeed res
                )

        decodePWithT tVal =
            case tVal of
                "log" ->
                    Decode.map AuditLog <| Decode.field "p" Decode.string

                "fail" ->
                    Decode.map AuditFail <| Decode.field "p" Decode.string

                "success" ->
                    Decode.map AuditSuccess <|
                        Decode.field "p" <|
                            (decode BallotResult
                                |> required "nVotes" int
                                |> required "totals" (procSuccessKVs <| Decode.keyValuePairs string)
                            )

                _ ->
                    Decode.succeed <| AuditLogErr <| "Unable to decode msg from auditor with type: " ++ tVal

        auditValDecoder =
            Decode.andThen
                (\tVal -> decodePWithT tVal)
            <|
                Decode.field "t" string
    in
    case Decode.decodeValue auditValDecoder auditVal of
        Ok auditMsg ->
            FromAuditor auditMsg

        Err err ->
            errHelper "Unable to decode message from auditor! check console.log " err


gotAuditMsg : Sub Msg
gotAuditMsg =
    gotAuditMsgImpl decodeAuditMsg
