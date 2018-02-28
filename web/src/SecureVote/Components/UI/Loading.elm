module SecureVote.Components.UI.Loading exposing (..)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class, id)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)


loadingSpinner : String -> Html Msg
loadingSpinner msg =
    div [ id "loading-screen" ]
        [ text msg
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
