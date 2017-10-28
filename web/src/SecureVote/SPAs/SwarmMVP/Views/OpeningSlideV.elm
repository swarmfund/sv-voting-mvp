module SecureVote.SPAs.SwarmMVP.Views.OpeningSlideV exposing (..)

import Html exposing (Html, a, div, em, p, span, text)
import Html.Attributes exposing (class, href, style)
import Material.Card as Card
import Material.Color as Color exposing (Hue(Red), Shade(S500))
import Material.Options as Options exposing (cs, css)
import Material.Typography exposing (body1, display1, display2, display3, headline, title)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))


openingSlide : Model -> Html Msg
openingSlide model =
    let
        introText =
            [ [ text "This ballot is to decide the release schedule for SWM tokens." ]
            , [ text "You will be presented with a number of options in the form "
              , em [] [ text "X releases over Y days." ]
              , text " If X were 8, this would mean that tokens will be released in 8 distinct events, with each releasing 12.5% of all tokens. If Y were 42, that means that each release event will occur every 42 days. Each option has a 'details' button that will give you specifc information about the proposed release schedule."
              ]
            , [ text "When you vote, you allocate each option a number from -3 to +3 (inclusive). It's important to choose a vote for each option. (This method of voting is called 'Range Voting'.)" ]
            , [ text "If you'd like, ", a [ href "https://youtube.com" ] [ text "here is a video" ], text " walking you through the voting process." ]
            , [ text "When you're ready, let's begin the voting process!" ]
            ]

        introParagraphs =
            div [ class "center", style [ ( "max-width", "700px" ) ] ] <| List.map renderPara introText

        renderPara txt =
            Options.styled span [ body1, cs "black db pa2 mv2" ] txt
    in
    fullPageSlide 9483579329
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled div [ display2, Color.text Color.black, cs "pa2 heading-text" ] [ text "Welcome to the Swarm Release Schedule Vote" ]
            , Options.styled div [ headline, cs "black pa2 mv3" ] [ text "This vote is open to all Swarm token holders." ]
            , introParagraphs
            , div [ class "mv3" ]
                [ ballotIntegrity model
                    [ btn 348739845 model [ PriBtn, Attr (class "ph2"), Click (PageGoForward SwmAddressR) ] [ text "Continue" ]
                    ]
                ]
            ]
        ]


ballotIntegrity : Model -> List (Html Msg) -> Html Msg
ballotIntegrity model successHtml =
    let
        verifyingHtml =
            Options.styled
                div
                [ headline, Color.text Color.black, cs "mv2" ]
                [ text "Verifying Integrity of Vote..." ]

        ballotVerificationPassed =
            Just False

        failHtml errMsg =
            div []
                [ Options.styled
                    div
                    [ headline, Color.text <| Color.color Red S500 ]
                    [ text "Verification Failed!" ]
                , Options.styled
                    p
                    [ body1, cs "black db pa2 mv2" ]
                    [ text "Failure reason: "
                    , text errMsg
                    ]
                ]

        rendered =
            case ballotVerificationPassed of
                Just True ->
                    successHtml

                Just False ->
                    [ failHtml <| model.verificationError ? "Verification error not found :(" ]

                _ ->
                    [ verifyingHtml ]
    in
    div [ class "mv2" ] rendered
