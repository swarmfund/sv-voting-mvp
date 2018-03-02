module SecureVote.SPAs.DelegationUI.Helpers exposing (..)

import Dict
import SecureVote.SPAs.DelegationUI.Model exposing (Model)


getStrField : Model -> String -> Maybe String
getStrField m k =
    Dict.get k m.strFields


getBoolField : Model -> String -> Maybe Bool
getBoolField m k =
    Dict.get k m.boolFields
