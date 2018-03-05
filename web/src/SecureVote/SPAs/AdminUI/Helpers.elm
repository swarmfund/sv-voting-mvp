module SecureVote.SPAs.AdminUI.Helpers exposing (..)

import Dict
import Json.Encode as E exposing (Value)
import SecureVote.SPAs.AdminUI.Model exposing (Model)


getStrField : Model -> String -> Maybe String
getStrField m k =
    Dict.get k m.strFields


getBoolField : Model -> String -> Maybe Bool
getBoolField m k =
    Dict.get k m.boolFields


genDeployArgs : { democHash : String, bHash : String, extraData : String, openPeriod : ( Int, Int ), useEnc : Bool } -> Value
genDeployArgs { democHash, bHash, extraData, openPeriod, useEnc } =
    let
        ( sTs, eTs ) =
            openPeriod
    in
    E.list
        [ E.string democHash
        , E.string bHash
        , E.string extraData
        , E.list
            [ E.int sTs
            , E.int eTs
            ]
        , E.list
            [ E.bool useEnc --^ use encryption
            , E.bool False --^ testing
            ]
        ]
