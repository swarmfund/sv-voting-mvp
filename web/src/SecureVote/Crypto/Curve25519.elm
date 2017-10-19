port module SecureVote.Crypto.Curve25519 exposing (..)

import Json.Decode exposing (Value, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)


port receiveCurve25519Error : (Value -> msg) -> Sub msg


onIncomingCurve25519Error : (String -> msg) -> Value -> msg
onIncomingCurve25519Error errMsg value =
    Json.Decode.decodeValue string value
        |> Result.withDefault "Failed to decode error string from Curve25519 operations..."
        |> errMsg



-- key pair generation


port genKeyPair : Bool -> Cmd msg


port receiveKeyPair : (Value -> msg) -> Sub msg


onIncomingKeyPair : (Curve25519KeyPair -> msg) -> (String -> msg) -> Value -> msg
onIncomingKeyPair successMsg errMsg encKeyPair =
    let
        decResult =
            decode Curve25519KeyPair
                |> required "hexPk" string
                |> required "hexSk" string
    in
    case Json.Decode.decodeValue decResult encKeyPair of
        Ok kp ->
            successMsg kp

        Err str ->
            errMsg str


type alias Curve25519KeyPair =
    { hexPk : String
    , hexSk : String
    }



-- encryption


port encryptBytes : { hexSk : String, hexRemotePk : String, bytesToSign : List Int } -> Cmd msg


port receiveEncryptedBytes : (Value -> msg) -> Sub msg


onIncomingEncBytes : (List Int -> msg) -> (String -> msg) -> Value -> msg
onIncomingEncBytes successMsg errMsg encBytes =
    let
        decoded =
            Json.Decode.decodeValue (list int) encBytes
    in
    case decoded of
        Ok encBs ->
            successMsg encBs

        Err str ->
            errMsg str
