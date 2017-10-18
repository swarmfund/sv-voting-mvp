module SecureVote.SPAs.SwarmMVP.Views.DialogV exposing (..)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (menu)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getEthNodeTemp, setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(SetProvider))


settingsDialogV : Model -> Html Msg
settingsDialogV model =
    let
        setEthNodeMsgs model_ =
            MultiMsg
                [ SetEthNode <| getEthNodeTemp model_ ? ""
                , ToWeb3 SetProvider
                ]
    in
    div []
        [ div []
            [ Options.styled span [ menu, cs "" ] [ text "Ethereum Node URL" ]
            , Textf.render Mdl
                [ 7785646743 ]
                model.mdl
                [ Options.onInput <| setEthNodeTemp
                , Textf.value <| getEthNodeTemp model ? ""

                -- Seems to be an issue with the label not disappearing properly.
                --                , Textf.label initModel.ethNode
                , cs "mh4"
                , css "min-width" "400px"
                ]
                []
            , btn 456467568 model [ PriBtn, Click <| setEthNodeMsgs model ] [ text "Update" ]
            ]
        ]


infoDialogV : Html Msg
infoDialogV =
    div [] [ text "This is info Dialog Box" ]


gethDialogV : Html Msg
gethDialogV =
    div [] [ text "This is Geth Dialog Box" ]


verifyDialogV : Html Msg
verifyDialogV =
    div [] [ text "This is verify ballot dialog box" ]


customDialogV : String -> Html Msg
customDialogV content =
    div [] [ text content ]


debugDialogV : Model -> Html Msg
debugDialogV model =
    let
        liE str =
            li [] [ text str ]
    in
    ul [ class "" ]
        [ liE "Add whatever you want here for debug"
        , liE <| toString model.ballotBits
        ]
