port module SecureVote.Crypto.Curve25519 exposing (..)

import Json.Decode exposing (Value, int, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import SecureVote.Crypto.Types exposing (PrivKey)


port genKeyPair : Bool -> Cmd msg


port receiveKeyPair : (Value -> msg) -> Sub msg


onIncomingKeyPair : (ReceiveKeyPair -> msg) -> (String -> msg) -> Value -> msg
onIncomingKeyPair successMsg errMsg encKeyPair =
    let
        decResult =
            decode ReceiveKeyPair
                |> required "array" (list int)
                |> required "hex" string
    in
    case Json.Decode.decodeValue decResult encKeyPair of
        Ok kp ->
            successMsg kp

        Err str ->
            errMsg str


type alias ReceiveKeyPair =
    { array : List Int
    , hex : String
    }
