port module SecureVote.Eth.Web3 exposing (..)

import Debug
import Decimal
import Json.Decode as Decode exposing (Decoder, Value, int, string)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))


port setWeb3Provider : String -> Cmd msg


type alias GetErc20BalanceReq =
    { contractAddress : String, userAddress : String }


port getErc20Balance : GetErc20BalanceReq -> Cmd msg


port implErc20Balance : (Value -> msg) -> Sub msg


port gotWeb3Error : (Value -> msg) -> Sub msg


type alias ConsDataParamReq =
    { encBallot : String, voterPubkey : String, votingContractAddr : String }


port constructDataParam : ConsDataParamReq -> Cmd msg


port implDataParam : (Value -> msg) -> Sub msg


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


onIncomingWeb3Error : Value -> Msg
onIncomingWeb3Error err =
    case Decode.decodeValue string err of
        Ok err ->
            LogErr err

        Err _ ->
            let
                errStr =
                    toString err
            in
            LogErr (Debug.log errStr <| "Unable to decode error!!! Check console.log: " ++ errStr)


onRecieveDataParam : Value -> Msg
onRecieveDataParam dataVal =
    case Decode.decodeValue string dataVal of
        Ok data ->
            FromWeb3 <| GotDataParam data

        Err err ->
            let
                errStr =
                    toString err
            in
            LogErr (Debug.log errStr <| "Unable to decode data param from web3! Check console.log: " ++ errStr)
