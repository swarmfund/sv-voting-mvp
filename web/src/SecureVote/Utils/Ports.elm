module SecureVote.Utils.Ports exposing (..)

import Json.Decode as D exposing (Value)
import Json.Decode.Pipeline as P
import Json.Encode as E


type alias CarryPack =
    { hops : Int
    , payload : Value
    }


mkCarry v =
    CarryPack 0 v


carryPackDecoder : D.Decoder CarryPack
carryPackDecoder =
    P.decode CarryPack
        |> P.required "hops" D.int
        |> P.required "payload" D.value
