port module SecureVote.Crypto.Hashing exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Result.Extra exposing (unpack)


-- Types


type HashFmt
    = Hex
    | EthHex
    | String
    | FmtUnknown


fmtFromString : String -> HashFmt
fmtFromString s =
    case String.toLower s of
        "hex" ->
            Hex

        "ethhex" ->
            EthHex

        "string" ->
            String

        _ ->
            FmtUnknown


type HashAlg
    = Sha256
    | Keccak256
    | NoHash


algFromString : String -> HashAlg
algFromString s =
    case String.toLower s of
        "sha256" ->
            Sha256

        "keccak256" ->
            Keccak256

        _ ->
            NoHash


type GeneralHash
    = Hash HashAlg String


type alias HashParams =
    { input : HashFmt, output : HashFmt, alg : HashAlg }


type alias ToHashCmd =
    { input : String, output : String, toHash : String }


type alias FromHashCmd =
    { output : HashFmt, result : String, alg : HashAlg }



-- Main hash function


hash : HashParams -> String -> Cmd msg
hash { input, output, alg } toHash =
    case alg of
        Sha256 ->
            doHashSha256 { input = toString input, output = toString output, toHash = toHash }

        _ ->
            Cmd.none



-- Ports


port doHashSha256 : ToHashCmd -> Cmd msg


port gotHash : (Value -> msg) -> Sub msg


hashSubs : (HashMsg -> msg) -> Sub msg
hashSubs f =
    gotHash
        (\val ->
            let
                procFromHashCmd output result alg =
                    FromHashCmd (fmtFromString output) result (algFromString alg)

                decoder =
                    decode procFromHashCmd
                        |> required "output" string
                        |> required "result" string
                        |> required "alg" string
            in
            unpack (f << HashErr) (f << GotHash) <| decodeValue decoder val
        )



-- Model, Msg, and Update


type HashMsg
    = GotHash FromHashCmd
    | HashErr String
    | HashNop


type alias HashModel =
    String



-- { errHandler : String -> ( m, c ) } ->


hashUpdate : HashMsg -> HashModel -> ( HashModel, Cmd HashMsg )
hashUpdate msg model =
    case msg of
        GotHash { output, result, alg } ->
            ( result, Cmd.none )

        HashNop ->
            ( model, Cmd.none )

        HashErr m ->
            ( model, Cmd.none )



-- errHandler m
