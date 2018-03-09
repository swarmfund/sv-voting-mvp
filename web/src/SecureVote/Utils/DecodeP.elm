module SecureVote.Utils.DecodeP exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as P
import Result.Extra


reqIndex : Int -> D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
reqIndex i dec decoder =
    P.custom (D.index i dec) decoder


strInt =
    D.string |> D.andThen (D.decodeString D.int >> Result.Extra.unpack D.fail D.succeed)
