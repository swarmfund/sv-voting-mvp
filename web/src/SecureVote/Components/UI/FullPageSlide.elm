module SecureVote.Components.UI.FullPageSlide exposing (..)

import Dict
import Html exposing (Attribute, Html, code, div, li, text, ul)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(..))


-- TODO Refactor out SetElevation into SecureVote components (ala elm-mdl)


fullPageSlide : Int -> Model -> List (Attribute Msg) -> List (Card.Block Msg) -> Html Msg
fullPageSlide id model attrs innerHtmls =
    let
        backOpts =
            [ Icon, Attr (class "fl sv-button-large"), Click PageGoBack ]
                ++ (if List.length model.history == 0 then
                        [ Disabled ]
                    else
                        [ BtnNop ]
                   )

        settingsOpts =
            [ Icon, Attr (class "sv-button-large"), OpenDialog, Click (SetDialog "Settings" SettingsDialog) ]

        infoOpts =
            [ Icon, Attr (class "sv-button-large"), OpenDialog, Click (SetDialog "Info" InfoDialog) ]
    in
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
                [ btn 885338576 model backOpts [ Icon.view "arrow_back" [ Icon.size24 ] ]
                , div [ class "fr dib" ]
                    [ btn 133498391 model [ SecBtn, Click (SetDialog "Debug" DebugDialog), OpenDialog ] [ text "Debug" ]
                    , btn 130572984 model infoOpts [ Icon.view "info_outline" [ Icon.size24 ] ]
                    , btn 345647875 model settingsOpts [ Icon.view "settings" [ Icon.size24 ] ]
                    ]
                ]
             ]
                ++ innerHtmls
            )
        , div
            [ class "dtc w-10" ]
            []
        ]
