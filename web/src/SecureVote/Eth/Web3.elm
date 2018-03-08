port module SecureVote.Eth.Web3 exposing (..)

import Debug
import Decimal exposing (Decimal, fromString)
import Json.Decode as Decode exposing (Decoder, Value, bool, decodeValue, int, string)
import Json.Decode.Pipeline exposing (decode, required)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (combine)
import SecureVote.Eth.Types exposing (..)
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
    { ballot : String, useEnc : Bool, voterPubkey : String, votingContractAddr : String, abi : String }



-- # Arbitrary Contract Reads and Writes


port performContractRead : ReadContractDoc -> Cmd msg


port contractReadResponse : (Value -> msg) -> Sub msg


onContractReadResponse : (ReadResponse -> msg) -> (String -> msg) -> Value -> msg
onContractReadResponse msgGen errMsg val =
    let
        _ =
            Debug.log ("contract read got back err: " ++ toString val) True

        decoder =
            decode ReadResponse
                |> required "success" Decode.bool
                |> required "resp" Decode.value
                |> required "errMsg" Decode.string
                |> required "method" Decode.string
                |> required "addr" Decode.string
    in
    case Decode.decodeValue decoder val of
        Ok secVal ->
            if secVal.success then
                msgGen secVal
            else
                errMsg secVal.errMsg

        Err err ->
            errMsg err



-- # ERC20 Operations


type alias GetErc20BalanceReq =
    { contractAddress : String, userAddress : String, chainIndex : String, delegationABI : String, delegationAddr : String }


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


port performContractWriteMM : WriteViaMMDoc -> Cmd msg


port metamaskTxidImpl : (Value -> msg) -> Sub msg


castMetaMaskTx : CandidateEthTx -> Cmd msg
castMetaMaskTx { from, to, value, data, gas } =
    castMetaMaskVoteImpl
        { from = withDefault "" from
        , to = withDefault "ERROR - THIS SHOULD BE FILLED AND YOU SHOULD NEVER SEE THIS" to
        , value = value
        , data = withDefault "ERROR - THIS SHOULD BE FILLED AND YOU SHOULD NEVER SEE THIS" data
        , gas = gas
        }


metamaskTxid : Sub Msg
metamaskTxid =
    metamaskTxidGen
        (\r ->
            case r of
                Ok txid ->
                    FromWeb3 <| GotMetaMaskTxid txid

                Err err ->
                    errHelper "MetaMask returned an error: " err
        )


metamaskTxidGen : (Result String String -> msg) -> Sub msg
metamaskTxidGen msg =
    metamaskTxidImpl <|
        \val -> msg <| decodeValue string val


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


type alias GetDemocHashesReq =
    { indexABI : String, indexAddr : String, democHash : String, ballotBoxABI : String }


port getDemocHashes : GetDemocHashesReq -> Cmd msg


port democNBallots : (Value -> msg) -> Sub msg


port gotBallotInfo : (Value -> msg) -> Sub msg


port getErc20Abrv : { bHash : String, erc20Addr : String } -> Cmd msg


port gotErc20Abrv : (Value -> msg) -> Sub msg


port ballotInfoExtra : (Value -> msg) -> Sub msg


port getTxInfoContractWrite : GetTxInfoContractWrite -> Cmd msg


port gotTxInfo : (Value -> msg) -> Sub msg


handleGotTxInfo : (Result String String -> msg) -> Sub msg
handleGotTxInfo msgF =
    gotTxInfo
        (\v ->
            msgF <| decodeValue string v
        )



{- START DELEGATION SECTIONS -}


port setGlobalDelegationImpl : { delegationABI : String, contractAddr : String, delegateAddr : String } -> Cmd msg


port setTokenDelegationImpl : { delegationABI : String, contractAddr : String, delegateAddr : String, tokenContract : String } -> Cmd msg


port gotDelegatePayloadImpl : (Value -> msg) -> Sub msg


gotDelegatePayloadGen msg =
    gotDelegatePayloadImpl <|
        \val -> msg <| decodeValue string val



{- END DELEGATION SECTIONS -}
