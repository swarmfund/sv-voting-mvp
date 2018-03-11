module SecureVote.SPAs.DelegationUI.Helpers exposing (..)

import Dict
import Element.Input exposing (selected)
import Json.Encode as E
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Types exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(..))
import SecureVote.Tokens.Types exposing (tcChoiceToAddr)


getStrField : Model -> String -> Maybe String
getStrField m k =
    Dict.get k m.strFields


getBoolField : Model -> String -> Maybe Bool
getBoolField m k =
    Dict.get k m.boolFields


getBoolFieldWD : Model -> String -> Bool
getBoolFieldWD m k =
    Dict.get k m.boolFields |> Maybe.withDefault False


setDelegateAddrId =
    "setDelegateAddrId"


setDelegateTokenId =
    "setDelegateTokenId"


getDelegationVoterAddrId =
    "getDelegationVoterAddrId"


getDelegationTokenAddrId =
    "getDelegationTokenAddrId"


getVotersForDlgtId =
    "getVotersForDlgtId"


setDlgtCollapseId =
    "clpsSetDelegate"


txPrevCollapseId =
    "txPrevCollapseId"


viewDlgtCollapsedId =
    "viewDlgtCollapsedId"


viewVotersForDlgtCollapsedId =
    "viewVotersForDlgtCollapsedId"


getDlgtTypeId =
    "getDlgtTypeId"


ethCheckDelegationId =
    "ethCheckDelegationId"
