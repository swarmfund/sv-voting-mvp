module SecureVote.Components.UI.FullPageSlide exposing (..)

import Dict
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangeToPreviousPage, SetElevation))


-- TODO Refactor out SetElevation into SecureVote components (ala elm-mdl)


fullPageSlide : Int -> Model -> List (Attribute Msg) -> List (Card.Block Msg) -> Html Msg
fullPageSlide id model attrs innerHtmls =
    div [ class "dt w-100 mv5" ]
        [ div
            [ class "dtc w-10" ]
            []
        , Card.view
            [ cs "v-mid dtc center"
            , css "width" "auto"
            , Color.background (Color.color Color.Grey Color.S50)
            , if Dict.get id model.elevations ? False then
                Elevation.e8
              else
                Elevation.e2
            , Options.onMouseEnter (SetElevation id True)
            , Options.onMouseLeave (SetElevation id False)
            , Elevation.transition 125
            ]
            ([ Card.actions []
                [ btn 885338576 model [ Icon, Attr (class "fl sv-button-large"), Click ChangeToPreviousPage ] [ Icon.view "arrow_back" [ Icon.size48 ] ]
                , btn 345647875 model [ Icon, Attr (class "fr sv-button-large") ] [ Icon.view "menu" [ Icon.size48 ] ]
                ]
             ]
                ++ innerHtmls
            )
        , div
            [ class "dtc w-10" ]
            []
        ]
