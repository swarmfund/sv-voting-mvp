module SecureVote.SPAs.DelegationUI.Views.RootV exposing (..)

import Element exposing (button, column, el, layout, row, text)
import Element.Attributes exposing (center, content, fill, height, padding, px, spacing, width)
import Element.Events exposing (onClick)
import Element.Input as I
import Html exposing (Html)
import Maybe.Extra exposing ((?))
import SecureVote.SPAs.DelegationUI.Components.Input exposing (textInput)
import SecureVote.SPAs.DelegationUI.Helpers exposing (getStrField)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(GetDelegationPayload, SetStrField))
import SecureVote.SPAs.DelegationUI.Views.Render exposing (renderPayload, renderTxPreview)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (AdminStyles(..), UiElem, stylesheet)


rootV : Model -> Html Msg
rootV model =
    layout stylesheet <|
        -- An el is the most basic element, like a <div>
        el NoS [ padding 20, width fill, height fill ]
        <|
            row NoS
                [ spacing 20 ]
                [ column NoS
                    [ spacing 20, width fill, height fill ]
                    [ el Title [] (text "SV.Light Delegation UI")
                    , delegationFields model
                    ]
                , column NoS
                    [ width <| px 30 ]
                    []
                , column NoS
                    [ spacing 20, width fill ]
                    [ el Title [] (text "Delegation Tx Preview")
                    , renderTxPreview model
                    , renderPayload model
                    ]
                ]



-- Radio Box for global or token delegation
-- TextField Delegate Address
-- Select Token Address


delegationFields : Model -> UiElem
delegationFields model =
    let
        getField id =
            getStrField model id ? ""

        typicalField id label =
            textInput
                { onChange = SetStrField id
                , value = getField id
                , label = I.labelAbove <| text label
                , options = []
                }

        onClickMsg =
            onClick <|
                GetDelegationPayload
                    { delegateAddr = getField delegateId
                    , tokenAddr = getField tokenId
                    }
    in
    column NoS
        [ spacing 20, width fill ]
        [ typicalField delegateId "Delegate Address"
        , typicalField tokenId "Token Address"
        , button Field [ onClickMsg, width content, center, padding 10 ] (text "Get Delegation Payload")
        ]


delegateId =
    "delegateId"


tokenId =
    "tokenId"
