module SecureVote.SPAs.SwarmMVP.Views.Common exposing (..)

import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class, classList, style)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getNBallots, getNBallotsVotedOn)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)


voteProgressBar : Model -> Html Msg
voteProgressBar model =
    let
        xWidth =
            nVotedDone * 100 // total |> toString

        nVotedDone =
            getNBallotsVotedOn model

        total =
            getNBallots model
    in
    div [ class "center relative mv0 w-100" ]
        [ div [ class "relative mv2 z-999", style [ ( "mix-blend-mode", "difference" ), ( "color", "#568ebd" ) ] ] [ text <| "Completed " ++ toString nVotedDone ++ " / " ++ toString total ++ " ballots." ]
        , div [ class "absolute h-100 top-0 z-0", style [ ( "width", xWidth ++ "%" ), ( "background-color", "#568ebd" ) ] ] []
        ]


addProgressToCardBody : Model -> List (Html Msg)
addProgressToCardBody model =
    [ hr [ class "mv0 pt0" ] []
    , voteProgressBar model
    ]
