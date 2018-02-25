module SecureVote.SPAs.SwarmMVP.Web3Handler exposing (..)

import Json.Decode as Decode exposing (Value, int)
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (RemoteData(Failure, Success))
import SecureVote.Eth.Web3 exposing (ReadResponse)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))


{-| Decode contract read msg |
-}
decodeRead : ReadResponse -> Msg
decodeRead { success, errMsg, response, method } =
    case method of
        "getBallotOptionsLegacy" ->
            if success then
                decodeBallotOptsLegacy response
            else
                MultiMsg [ LogErr errMsg, FromWeb3 <| GetBallotOptsLegacy (Failure errMsg) ]

        "getBallotOptions" ->
            if success then
                decodeBallotOpts response
            else
                MultiMsg [ LogErr errMsg, FromWeb3 <| GetBallotOpts (Failure errMsg) ]

        "ballotPeriod" ->
            if success then
                decodeBallotPeriod response
            else
                MultiMsg [ LogErr errMsg, FromWeb3 <| GetBallotPeriod (Failure errMsg) ]

        _ ->
            LogErr "Unknown method returned from web3"


decodeBallotPeriod : Value -> Msg
decodeBallotPeriod val =
    let
        decoder =
            decode (\s e -> { startTime = s, endTime = e })
                |> required "startTime" int
                |> required "endTime" int
    in
    case Decode.decodeValue decoder val of
        Ok bPeriod ->
            FromWeb3 <| GetBallotPeriod <| Success bPeriod

        Err msg ->
            FromWeb3 <| GetBallotPeriod <| Failure msg


decodeBallotOptsLegacy : Value -> Msg
decodeBallotOptsLegacy val =
    let
        llDecoder =
            Decode.list (Decode.list Decode.int)
    in
    case Decode.decodeValue llDecoder val of
        Ok opts ->
            FromWeb3 <| GetBallotOptsLegacy (Success opts)

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GetBallotOptsLegacy (Failure err) ]


decodeBallotOpts : Value -> Msg
decodeBallotOpts val =
    let
        llDecoder =
            Decode.bool
    in
    case Decode.decodeValue llDecoder val of
        Ok opts ->
            FromWeb3 <| GetBallotOpts (Success opts)

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GetBallotOpts (Failure err) ]


readOptsErr : String -> Msg
readOptsErr errMsg =
    MultiMsg [ LogErr errMsg ]
