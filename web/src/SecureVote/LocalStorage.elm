port module SecureVote.LocalStorage exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (Value, decodeValue, string)
import Json.Decode.Pipeline exposing (..)


port setLocalStorage : LsEntry -> Cmd msg


port getLocalStorage : String -> Cmd msg


port localStorageResp : (Value -> msg) -> Sub msg


port localStorageFail : (Value -> msg) -> Sub msg


localStorageSub : (Result String LsEntry -> msg) -> Sub msg
localStorageSub msg =
    localStorageResp
        (\v ->
            let
                decoder =
                    decode LsEntry
                        |> required "key" string
                        |> required "value" string
            in
            msg <| decodeValue decoder v
        )


localStorageErrSub : (Result String LsError -> msg) -> Sub msg
localStorageErrSub msg =
    localStorageFail
        (\v ->
            let
                decoder =
                    decode LsError
                        |> required "key" string
                        |> required "msg" string
            in
            msg <| decodeValue decoder v
        )


type alias LsEntry =
    { key : String, value : String }


type alias LsError =
    { key : String, errMsg : String }


type alias LsModel =
    Dict String String


type LsMsg
    = SetLocalStorage ( String, String )
    | GetLocalStorage String
    | LsGeneral ( String, String )
    | LsNotFound String


lsUpdate : LsMsg -> LsModel -> ( LsModel, Cmd LsMsg )
lsUpdate msg model =
    case msg of
        SetLocalStorage ( k, v ) ->
            model ! [ setLocalStorage { key = k, value = v } ]

        GetLocalStorage k ->
            model ! [ getLocalStorage k ]

        LsGeneral ( k, v ) ->
            Dict.insert k v model ! []

        LsNotFound k ->
            Dict.remove k model ! []
