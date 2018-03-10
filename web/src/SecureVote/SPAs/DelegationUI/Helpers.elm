module SecureVote.SPAs.DelegationUI.Helpers exposing (..)

import Dict
import Element.Input exposing (selected)
import Json.Encode as E
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Types exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(..))
import SecureVote.Tokens.Types exposing (tcChoiceToAddr)


setDelegationArgs : Model -> List E.Value
setDelegationArgs model =
    let
        dlgtArg =
            E.string <| getStrField model setDelegateAddrId ? "Error: no delegate selected"

        argList =
            case model.delType of
                Just Token ->
                    [ E.string <| tcChoiceToAddr (selected model.tokenConAddr)
                    , dlgtArg
                    ]

                Just Global ->
                    [ dlgtArg ]

                _ ->
                    [ E.string "Error: No Delegation Type Selected!" ]
    in
    argList


viewDelegationArgs : Model -> List E.Value
viewDelegationArgs model =
    let
        dlgtType =
            getStrField model getDlgtTypeId ? "none"

        tokenAddr =
            tcChoiceToAddr (selected model.tokenConAddr)

        tokenStr =
            if dlgtType == "global" then
                zeroAddr
            else
                tokenAddr
    in
    [ E.string <| getStrField model getDelegationVoterAddrId ? "Error: no voter address when checking delegation"
    , E.string tokenStr
    ]


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


setDlgtCollapseId =
    "clpsSetDelegate"


txPrevCollapseId =
    "txPrevCollapseId"


viewDlgtCollapsedId =
    "viewDlgtCollapsedId"


getDlgtTypeId =
    "getDlgtTypeId"
