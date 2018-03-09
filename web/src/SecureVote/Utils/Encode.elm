module SecureVote.Utils.Encode exposing (..)

import Dict exposing (Dict)
import Json.Encode as E exposing (Value)


encDict : (v -> Value) -> Dict String v -> Value
encDict encV =
    E.object << List.map (\( k, v ) -> ( k, encV v )) << Dict.toList


encDictDict : (v -> Value) -> Dict String (Dict String v) -> Value
encDictDict encV =
    encDict (encDict encV)
