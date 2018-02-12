module SecureVote.Components.UI.FullPageSlide exposing (..)

import Dict
import Html exposing (Attribute, Html, code, div, hr, li, text, ul)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.Typo exposing (headline)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(..))


-- TODO Refactor out SetElevation into SecureVote components (ala elm-mdl)


fullPageSlide : Int -> Model -> String -> List (Html Msg) -> Html Msg
fullPageSlide id model title innerHtmls =
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
    div [ class "db w-100 mv5", style [ ( "height", "90%" ) ] ]
        [ div
            [ class "dib w-10" ]
            []
        , Card.view
            [ cs "v-mid center dib w-80"
            , css "max-height" "100%"
            , Color.background (Color.color Color.Grey Color.S50)
            , if Dict.get id model.elevations ? False then
                Elevation.e8
              else
                Elevation.e2
            , Options.onMouseEnter (SetElevation id True)
            , Options.onMouseLeave (SetElevation id False)
            , Elevation.transition 125
            ]
          <|
            [ Card.actions []
                [ btn 885338576 model backOpts [ Icon.view "arrow_back" [ Icon.size24 ] ]
                , div [ class "fr dib" ]
                    [ btn 130572984 model infoOpts [ Icon.view "info_outline" [ Icon.size24 ] ]
                    , btn 345647875 model settingsOpts [ Icon.view "settings" [ Icon.size24 ] ]
                    ]
                ]
            , Card.text [ cs "center tc mb0 pb0" ] <|
                [ headline title
                , hr [ class "mb0" ] []
                , div [ class "overflow-y-scroll", style [ ( "max-height", "75vh" ) ] ] <|
                    [ div [ class "pt3" ] [] ] ++ innerHtmls ++ [ div [ class "pb3 mb1" ] [] ]
                ]
            ]
        , div
            [ class "dib w-10" ]
            []
        ]



-- btn 133498391 model [ SecBtn, Click (SetDialog "Debug" DebugDialog), OpenDialog ] [ text "Debug" ]
