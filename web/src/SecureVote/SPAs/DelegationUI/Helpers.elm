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


viewDelegationArgs : ( String, String ) -> List E.Value
viewDelegationArgs ( voterAddr, tokenAddr ) =
    [ E.string voterAddr
    , E.string tokenAddr
    ]


viewDelegatorsArgs : String -> List E.Value
viewDelegatorsArgs dlgtAddr =
    [ E.string <| dlgtAddr ]


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
