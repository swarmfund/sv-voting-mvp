module SecureVote.Components.UI.Loading exposing (..)

import Element exposing (Element, el, row, text)
import Element.Attributes exposing (center, height, px, verticalCenter, width)
import Html exposing (Html, div, li, span, ul)
import Html.Attributes exposing (class, id, title)
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(..))
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(LoadingIndStyle))
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)


loadingSpinner : String -> Html Msg
loadingSpinner msg =
    div [ id "loading-screen" ]
        [ Html.text msg
        , div [ class "cssload-container cssload-orange cssload-small" ]
            [ ul [ class "cssload-flex-container" ]
                [ li []
                    [ span [ class "cssload-loading cssload-one" ] []
                    , span [ class "cssload-loading cssload-two" ] []
                    , span [ class "cssload-loading-center" ] []
                    ]
                ]
            ]
        ]


loadingIndicator : (CommonStyle -> s) -> Maybe (RemoteData.RemoteData String String) -> Element s v e
loadingIndicator w rd =
    row (w LoadingIndStyle) [ width <| px 40, height <| px 40, center, verticalCenter ] <|
        List.singleton <|
            case rd ? NotAsked of
                Success _ ->
                    text "✅"

                Loading ->
                    text "⏳"

                NotAsked ->
                    text "➡️"

                Failure err ->
                    Element.html <| Html.span [ title err ] [ Html.text "⚠️" ]
