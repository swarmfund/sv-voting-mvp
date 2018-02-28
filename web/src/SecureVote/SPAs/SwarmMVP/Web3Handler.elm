module SecureVote.SPAs.SwarmMVP.Web3Handler exposing (..)

import Json.Decode as D exposing (Value, int)
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (RemoteData(Failure, Success))
import SecureVote.Eth.Types exposing (BallotInfo)
import SecureVote.Eth.Web3 exposing (ReadResponse, democNBallots, gotBallotInfo)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))


handleResponse : (RemoteData String a -> FromWeb3Msg) -> Result String a -> Msg
handleResponse msg res =
    case res of
        Ok s ->
            FromWeb3 <| msg <| Success s

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| msg <| Failure err ]


{-| D contract read msg |
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
    case D.decodeValue decoder val of
        Ok bPeriod ->
            FromWeb3 <| GetBallotPeriod <| Success bPeriod

        Err msg ->
            FromWeb3 <| GetBallotPeriod <| Failure msg


decodeBallotOptsLegacy : Value -> Msg
decodeBallotOptsLegacy val =
    let
        llDr =
            D.list (D.list D.int)
    in
    case D.decodeValue llDr val of
        Ok opts ->
            FromWeb3 <| GetBallotOptsLegacy (Success opts)

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GetBallotOptsLegacy (Failure err) ]


decodeBallotOpts : Value -> Msg
decodeBallotOpts val =
    let
        llDr =
            decode (\isGood hashes -> { isGood = isGood, hashes = hashes })
                |> required "isGood" D.bool
                |> required "hashes" (D.list D.string)
    in
    case D.decodeValue llDr val of
        Ok opts ->
            FromWeb3 <| GetBallotOpts (Success opts)

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GetBallotOpts (Failure err) ]


readOptsErr : String -> Msg
readOptsErr errMsg =
    MultiMsg [ LogErr errMsg ]


democNVotesSub : Sub Msg
democNVotesSub =
    democNBallots
        (\val ->
            let
                decoder =
                    decode (\democHash n -> { democHash = democHash, n = n })
                        |> required "democHash" D.string
                        |> required "n" D.int
            in
            case D.decodeValue decoder val of
                Ok ballotCount ->
                    FromWeb3 <| GotBallotCount (Success ballotCount)

                Err err ->
                    MultiMsg [ LogErr err, FromWeb3 <| GotBallotCount (Failure err) ]
        )


gotBallotInfoSub : Sub Msg
gotBallotInfoSub =
    gotBallotInfo
        (\val ->
            let
                decoder =
                    decode BallotInfo
                        |> required "democHash" D.string
                        |> required "i" D.int
                        |> required "specHash" D.string
                        |> required "extraData" D.string
                        |> required "votingContract" D.string
            in
            handleResponse GotBallotInfo <| D.decodeValue decoder val
        )
