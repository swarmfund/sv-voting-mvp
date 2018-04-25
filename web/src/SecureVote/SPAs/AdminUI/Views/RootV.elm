module SecureVote.SPAs.AdminUI.Views.RootV exposing (..)

import Element exposing (column, el, empty, h1, h2, layout, paragraph, row, subheading, table, text, textLayout, viewport)
import Element.Attributes exposing (alignBottom, content, fill, height, maxHeight, maxWidth, minWidth, padding, paddingBottom, paddingXY, percent, px, spacing, vary, width, xScrollbar, yScrollbar)
import Html exposing (Html)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.CommonStyles exposing (Variations(FSmall))
import SecureVote.SPAs.AdminUI.Fields exposing (democHashId)
import SecureVote.SPAs.AdminUI.Helpers exposing (getStrField)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.Views.BallotBuilder exposing (ballotBuilder)
import SecureVote.SPAs.AdminUI.Views.Render exposing (renderBallotHash, renderBallotSpec, renderEventLog, renderTxInfo)
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(..), UiElem, stylesheet)


rootV : Model -> Html Msg
rootV model =
    let
        democHash =
            getStrField model democHashId ? "No DemocHash Specified"

        {- [ row NoS [ height fill ] [], paragraph NoS [ xScrollbar ] [ text txt ] ] -}
        code txt =
            column Code
                [ xScrollbar, alignBottom, vary FSmall True, maxWidth <| percent 100 ]
                [ row NoS [ height fill ] [], paragraph NoS [ xScrollbar ] [ text txt ] ]

        networkName =
            if model.dev then
                "Kovan"
            else
                "MainNet"
    in
    viewport stylesheet <|
        -- An el is the most basic element, like a <div>
        el NoS [ height fill, padding 20 ]
        <|
            row NoS
                [ height fill ]
                [ column NoS
                    [ padding 10, spacing 20, maxWidth <| percent 57.5, width fill, height fill, yScrollbar ]
                    [ h1 Title [] (text "SecureVote Light Ballot Builder")
                    , table NoS
                        [ spacing 5 ]
                        [ [ text "Network", text "Democracy Index", text "Democracy ID" ]
                        , [ text networkName, code model.indexAddr, code democHash ]
                        ]
                    , ballotBuilder model
                    , column NoS [ height <| px 300 ] <| []
                    ]
                , column NoS
                    [ width <| px 40 ]
                    []
                , column NoS
                    [ padding 10, spacing 20, width fill, minWidth <| percent 40, yScrollbar ]
                    [ h1 Title [] (text "Ballot Spec Preview")
                    , renderBallotSpec model
                    , subheading NoS [] "Ballot Hash"
                    , renderBallotHash model
                    , subheading NoS [] "Tx Preview"
                    , renderTxInfo model
                    , subheading NoS [] "Event Log"
                    , renderEventLog model
                    , column NoS [ height <| px 300 ] <| []
                    ]
                ]
