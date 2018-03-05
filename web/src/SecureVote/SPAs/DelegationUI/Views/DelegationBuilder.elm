module SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Input as I
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (genDropSelect, radio, select, textInput)
import SecureVote.SPAs.DelegationUI.Helpers exposing (getStrField)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(GetDelegationPayload, SetDelegationType, SetStrField))
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(Global, Token), TokenContractChoice(Swarm), tcChoiceToAddr, tcChoiceToStr)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(ErrTxt, Field, NoS, SubMenu), UiElem, Variations(Disabled))


delegationFields : Model -> UiElem
delegationFields model =
    let
        onClickMsg =
            onClick <|
                GetDelegationPayload
                    { delegateAddr = getStrField model delegateId ? ""
                    , tokenAddr =
                        tcChoiceToAddr <|
                            if model.delType == Just Global then
                                Nothing
                            else
                                I.selected model.tokenConAddr
                    }

        isDisabled =
            if delAddrInvalid model || (selectTokenInvalid model && model.delType == Just Token) then
                [ attribute "disabled" "disabled" ]
            else
                []
    in
    column NoS
        [ spacing 20, width fill ]
        [ delegateAddrField model
        , radioButtons model
        , selectTokenContract model
        , button Field ([ onClickMsg, width content, center, padding 10 ] ++ isDisabled) (text "Get Delegation Payload")
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


radioButtons : Model -> UiElem
radioButtons model =
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



-- TODO: Update to Input.autocomplete instead of Input.select


selectTokenContract : Model -> UiElem
selectTokenContract model =
    let
        mkChoice c =
            I.choice c (text <| tcChoiceToStr c)
    in
    if model.delType == Just Token then
        row NoS
            []
            [ select
                { label = I.labelAbove <| text "Token Contract"
                , with = model.tokenConAddr
                , max = 5
                , options = [ I.errorBelow <| when (selectTokenInvalid model) <| el ErrTxt [] (text "Please Select a Token Contract") ]
                , menu =
                    I.menu SubMenu
                        []
                        [ mkChoice Swarm
                        ]
                }
            ]
    else
        empty


selectTokenInvalid : Model -> Bool
selectTokenInvalid model =
    I.selected model.tokenConAddr == Nothing


delegateId =
    "delegateId"


tokenId =
    "tokenId"
