module SecureVote.SPAs.DelegationUI.Views.RootV exposing (..)

import Element exposing (column, el, layout, row, text)
import Element.Attributes exposing (fill, height, padding, px, spacing, width)
import Html exposing (Html)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (delegationBuilder)
import SecureVote.SPAs.DelegationUI.Views.Render exposing (renderBallotHash, renderBallotSpec)
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
                    [ el Title [] (text "LittleGov Delegation UI")
                    , delegationBuilder model
                    ]
                , column NoS
                    [ width <| px 30 ]
                    []
                , column NoS
                    [ spacing 20, width fill ]
                    [ el Title [] (text "Delegation Spec Preview")
                    , renderBallotSpec model
                    , renderBallotHash model
                    ]
                ]
