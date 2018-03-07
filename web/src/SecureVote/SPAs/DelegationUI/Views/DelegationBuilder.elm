module SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Input as I
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.CommonStyles exposing (Variations(Disabled))
import SecureVote.Components.UI.StatusMsgs exposing (warning)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (radio, select, textInput)
import SecureVote.SPAs.DelegationUI.Helpers exposing (delegateId, getStrField)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(OnFieldUpdate, SetDelegationType, SetStrField))
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(..))
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(..), UiElem)
import SecureVote.Tokens.Types exposing (TokenContract(..), tcChoiceToAddr, tcChoiceToStr)


delegationFields : Model -> UiElem
delegationFields model =
    let
        onClickMsg =
            onClick OnFieldUpdate

        isDisabled =
            delAddrInvalid model || (selectTokenInvalid model && model.delType == Just Token)

        optWarn =
            if isDisabled then
                warning CS "Delegation form has problems!"
            else
                empty
    in
    column NoS
        [ spacing 20, width fill ]
        [ delegateAddrField model
        , globalOrTokenChoice model
        , selectTokenContract model
        , optWarn
        ]


delegateAddrField : Model -> UiElem
delegateAddrField model =
    row NoS
        []
        [ textInput
            { onChange = SetStrField delegateId
            , value = getStrField model delegateId ? ""
            , label = I.labelAbove <| text "Delegate Address"
            , options =
                [ I.errorBelow <| when (delAddrInvalid model) <| el ErrTxt [] (text "Please enter a valid address") ]
            }
        ]


delAddrInvalid : Model -> Bool
delAddrInvalid model =
    let
        value =
            getStrField model delegateId ? ""
    in
    not <| isValidEthAddress value


globalOrTokenChoice : Model -> UiElem
globalOrTokenChoice model =
    row NoS
        []
        [ radio
            { onChange = SetDelegationType
            , selected = model.delType
            , label = I.labelAbove (text "Type of Delegation")
            , options = []
            , choices =
                [ I.choice Global (text "Global")
                , I.choice Token (text "Token")
                ]
            }
        ]


selectTokenContract : Model -> UiElem
selectTokenContract model =
    let
        mkChoice c =
            I.choice c (text <| tcChoiceToStr c)

        mkMenu cs =
            I.menu SubMenu [] <| List.map mkChoice cs
    in
    if model.delType == Just Token then
        row NoS
            [ width fill ]
            [ select
                { label = I.labelAbove <| el NoS [ width fill ] (text "Token Contract")
                , with = model.tokenConAddr
                , max = 5
                , options = [ I.errorBelow <| when (selectTokenInvalid model) <| el ErrTxt [] (text "Please Select a Token Contract") ]
                , menu =
                    mkMenu
                        [ Swarm
                        ]
                }
            ]
    else
        empty


selectTokenInvalid : Model -> Bool
selectTokenInvalid model =
    I.selected model.tokenConAddr == Nothing
