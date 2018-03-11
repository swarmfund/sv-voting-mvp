module SecureVote.Eth.Tasks exposing (..)

import Json.Decode as Decode exposing (Decoder, Value, decodeValue, string, value)
import Json.Decode.Pipeline exposing (decode, required)
import Native.Eth
import Result.Extra
import SecureVote.Eth.Types exposing (ReadContractDoc, ReadResponse)
import Task exposing (Task)


type alias Web3Ref =
    Value


decodeAndUnpackResp decoder =
    Task.andThen (\r -> Result.Extra.unpack Task.fail Task.succeed (decodeValue decoder r.response))


readResponseDec : Decoder ReadResponse
readResponseDec =
    decode ReadResponse
        |> required "resp" value
        |> required "method" string
        |> required "addr" string



{- init this first by passing in the Web3Ref -}


ethTasksInit : Web3Ref -> Task Never ()
ethTasksInit web3 =
    Native.Eth.init web3


readContract : ReadContractDoc -> Task String ReadResponse
readContract =
    Native.Eth.readContract
        >> Task.andThen (\r -> Result.Extra.unpack Task.fail Task.succeed (decodeValue readResponseDec r))
