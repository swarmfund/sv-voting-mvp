module SecureVote.SPAs.SwarmMVP.Views.SwmAddressV exposing (..)

import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (css, cs)
import Material.Textfield as Textf
import Material.Typography exposing (body1, display1, display2, display3, headline, title)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangePage))
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl, SetField))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmVoteR))
import Maybe.Extra exposing (isNothing, (?))
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getSwmAddress, isValidEthAddress, setSwmAddress, swmAddrId)
import Tuple exposing (second)


swmAddressV : Model -> Html Msg
swmAddressV model =
    let
        ( addrErr, addrErrMsg ) =
            validAddress model

        btnDisabled =
            if addrErr || (isNothing <| getSwmAddress model) then
                Disabled
            else
                BtnNop
    in
        fullPageSlide 384938493
            model
            []
            [ Card.text [ cs "center tc" ]
                [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
                , Options.styled span [ headline, cs "black db pa2 mv3" ] [ text "Please enter your Swarm address below" ]
                , div [ class "center" ]
                    [ div [ class "flex flex-column items-center" ]
                        [ div [ class "flex flex-column items-start" ]
                            [ Textf.render Mdl
                                [ 83543983 ]
                                model.mdl
                                [ Options.onInput <| setSwmAddress
                                , Textf.label "Your Ethereum Address"
                                , Textf.floatingLabel
                                , Textf.value <| getSwmAddress model ? ""
                                , Textf.error (addrErrMsg) |> Options.when (addrErr)
                                , css "min-width" "400px"
                                ]
                                []
                            ]
                        ]
                    , div [ class "mv3" ]
                        [ btn 894823489 model [ PriBtn, Attr (class "ph2"), Click (ChangePage SwmVoteR), btnDisabled ] [ text "Continue" ]
                        ]
                    ]
                ]
            ]


validAddress : Model -> ( Bool, String )
validAddress model =
    let
        swmAddress =
            getSwmAddress model
    in
        case swmAddress of
            Nothing ->
                ( False, "Please paste in your Eth address" )

            Just addr ->
                if isValidEthAddress addr then
                    ( False, "Address valid!" )
                else
                    ( True, "Invalid address" )
