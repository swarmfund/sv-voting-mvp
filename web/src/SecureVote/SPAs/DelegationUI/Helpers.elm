module SecureVote.SPAs.DelegationUI.Helpers exposing (..)

import Dict
import SecureVote.SPAs.DelegationUI.Model exposing (Model)


getStrField : Model -> String -> Maybe String
getStrField m k =
    Dict.get k m.strFields


getBoolField : Model -> String -> Maybe Bool
getBoolField m k =
    Dict.get k m.boolFields


getBoolFieldWD : Model -> String -> Bool
getBoolFieldWD m k =
    Dict.get k m.boolFields |> Maybe.withDefault False


delegateId =
    "delegateId"


tokenId =
    "tokenId"


setDlgtCollapseId =
    "clpsSetDelegate"


txPrevCollapseId =
    "txPrevCollapseId"


viewDlgtCollapsedId =
    "viewDlgtCollapsedId"
