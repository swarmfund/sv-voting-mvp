module SecureVote.SPAs.AdminUI.Views.RootV exposing (..)

import Element exposing (column, el, layout, row, text)
import Element.Attributes exposing (fill, height, padding, px, spacing, width)
import Html exposing (Html)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.Views.BallotBuilder exposing (ballotBuilder)
import SecureVote.SPAs.AdminUI.Views.Render exposing (renderBallotHash, renderBallotSpec)
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(..), UiElem, stylesheet)


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
                    [ el Title [] (text "LittleGov Ballot Builder")
                    , ballotBuilder model
                    ]
                , column NoS
                    [ width <| px 30 ]
                    []
                , column NoS
                    [ spacing 20, width fill ]
                    [ el Title [] (text "Ballot Spec Preview")
                    , renderBallotSpec model
                    , renderBallotHash model
                    ]
                ]
