module SecureVote.SPAs.SwarmMVP.Views.SetDelegateV exposing (..)

import Html exposing (Html, p, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (display2)
import Maybe.Extra exposing ((?), isNothing)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getDelegateAddress, setDelegateAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToCurve25519Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


delegateExplanationCopy : Model -> List String
delegateExplanationCopy model =
    [ "You may optionally select a delegate, though this isn't required. If you would like to, please enter their " ++ model.currentBallot.erc20Abrv ++ " token address below. "
    , "If your delegate casts a vote, your votes will be replaced with the votes that your delegate chooses. "
    , "If your delegate does not cast a vote, your vote will be cast with the options you have selected. "
    ]


delegateView : Model -> Html Msg
delegateView model =
    let
        btnDisabled =
            if addrErr then
                Disabled
            else
                BtnNop

        ( addrErr, addrErrMsg ) =
            validAddress model

        clickMsgs =
            MultiMsg
                [ ConstructBallotPlaintext
                , PageGoForward SwmSubmitR
                ]
    in
    fullPageSlide 3453456456
        model
        []
        [ Card.text [ cs "center tc" ]
            [ headline "Choose a Delegate"
            , p [ class "mw7 center" ] [ text <| String.concat <| delegateExplanationCopy model ]
            , Textf.render Mdl
                [ 7674564333 ]
                model.mdl
                [ Options.onInput <| setDelegateAddress
                , Textf.label "Delegate's Address (optional)"
                , Textf.floatingLabel
                , Textf.value <| getDelegateAddress model ? ""
                , Textf.error addrErrMsg |> Options.when addrErr
                , css "min-width" "400px"
                , cs "db center"
                ]
                []
            , btn 5475855442 model [ PriBtn, Attr (class "mv3"), Click clickMsgs, btnDisabled ] [ text "Continue" ]
            ]
        ]


validAddress : Model -> ( Bool, String )
validAddress model =
    let
        delegateAddress =
            getDelegateAddress model
    in
    case delegateAddress of
        Nothing ->
            ( False, "Please paste in your Delegate's Eth address" )

        Just addr ->
            if isValidEthAddress addr then
                ( False, "Address valid!" )
            else if addr == "" then
                ( False, "You will have no delegate" )
            else
                ( True, "Invalid address" )
