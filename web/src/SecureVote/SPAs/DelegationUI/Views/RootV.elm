module SecureVote.SPAs.DelegationUI.Views.RootV exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (delegationFields)
import SecureVote.SPAs.DelegationUI.Views.Render exposing (renderPayload, renderTxPreview)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (..)


rootV : Model -> Html Msg
rootV model =
    layout stylesheet <|
        -- An el is the most basic element, like a <div>
        el NoS [ padding 20, width fill, height fill ]
        <|
            row NoS
                [ spacing 20 ]
                [ column NoS
                    [ spacing 20, width (px 400) ]
                    [ el Title [] (text "SV.Light Delegation UI")
                    , delegationFields model
                    ]
                , column NoS
                    [ spacing 20 ]
                    [ el Title [] (text "Delegation Tx Preview")
                    , renderTxPreview model
                    , renderPayload model
                    ]
                ]
