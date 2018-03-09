port module SecureVote.Ballots.SpecSource exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import SecureVote.Ballots.Decoders exposing (handleBSpec)
import SecureVote.Ballots.Types exposing (BallotSpec(BVer01))
import String.Extra exposing (replace)


type CidType
    = Sha256


port getSpecFromIpfs : { id : String, cidHex : String } -> Cmd msg


port gotSpecFromIpfs : (Value -> msg) -> Sub msg


port gotFailedSpecFromIpfs : (Value -> msg) -> Sub msg


getBallotSpec : { bHash : String, cidType : CidType } -> Cmd msg
getBallotSpec { bHash, cidType } =
    let
        hashNoPrefix =
            replace "0x" "" bHash

        b58Prefix =
            case cidType of
                Sha256 ->
                    "1220"
    in
    getSpecFromIpfs { id = bHash, cidHex = b58Prefix ++ hashNoPrefix }


type alias SpecFromIpfs =
    { bHash : String, bSpec : BallotSpec, cid : String }


type alias FailSpecFromIpfs =
    { bHash : String, err : String }


gotSpecFromIpfsHandler : (Result String SpecFromIpfs -> msg) -> Sub msg
gotSpecFromIpfsHandler bMsg =
    gotSpecFromIpfs
        (\val ->
            let
                bSpecDecoder : Decoder BallotSpec
                bSpecDecoder =
                    resolve <|
                        (decode
                            (\version inner ->
                                case handleBSpec version inner of
                                    Ok v ->
                                        succeed v

                                    Err e ->
                                        fail e
                            )
                            |> required "ballotVersion" int
                            |> required "ballotInner" value
                        )

                decoder =
                    decode SpecFromIpfs
                        |> required "id" string
                        |> required "block" bSpecDecoder
                        |> required "cid" string
            in
            bMsg <| decodeValue decoder val
        )


gotFailedSpecFromIpfsHandler : (Result String FailSpecFromIpfs -> msg) -> Sub msg
gotFailedSpecFromIpfsHandler fMsg =
    let
        handler val =
            let
                decoder =
                    decode FailSpecFromIpfs
                        |> required "id" string
                        |> required "err" string
            in
            fMsg <| decodeValue decoder val
    in
    gotFailedSpecFromIpfs handler
