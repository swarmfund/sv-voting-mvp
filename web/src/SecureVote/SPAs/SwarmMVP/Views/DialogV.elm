module SecureVote.SPAs.SwarmMVP.Views.DialogV exposing (..)

import Html exposing (Html, a, b, div, img, li, p, pre, span, text, ul)
import Html.Attributes exposing (class, href, src)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (headline, menu)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml, dialogHtmlRender)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getEthNodeTemp, setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(SetProvider))
import SecureVote.SPAs.SwarmMVP.Views.SwmHowToVoteV exposing (combinedHowToVoteCopy)
import SecureVote.SPAs.SwarmMVP.Views.SwmVotingV exposing (candTxText)


settingsDialogV : Model -> Html Msg
settingsDialogV model =
    let
        setEthNodeMsgs =
            MultiMsg
                [ SetEthNode <| getEthNodeTemp model ? ""
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
            , btn 456467568 model [ PriBtn, Click <| setEthNodeMsgs ] [ text "Update" ]
            ]
        ]


infoDialogV : Html Msg
infoDialogV =
    let
        codeSourceCopy =
            [ "This voting tool was built by SecureVote for Swarm Fund. "
            , "The source code can be found at https://github.com/swarmfund/sv-voting-mvp. "
            , "All votes are validated through an auditing sutite."
            , "Instructions on how to run your own version of the auditing suite can be found at https://github.com/swarmfund/sv-voting-mvp. "
            ]

        codeSourceSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "About this voting tool" ]
                , text "This voting tool was built by SecureVote for Swarm Fund. The source code can be found at "
                , a [ href "https://github.com/swarmfund/sv-voting-mvp" ] [ text "https://github.com/swarmfund/sv-voting-mvp" ]
                , text ". All votes are validated through an auditing sutite. Instructions on how to run your own version of the auditing suite can be found at "
                , a [ href "https://github.com/swarmfund/sv-voting-mvp" ] [ text "https://github.com/swarmfund/sv-voting-mvp" ]
                , div [ class "mt4 tc" ]
                    [ img [ src "img/SecureVote.svg", class "w-30" ] []
                    , img [ src "img/SwarmFund.svg", class "w-40 ml4 v-btm" ] []
                    ]
                ]
    in
    div [] <|
        combinedHowToVoteCopy
            ++ [ codeSourceSection ]



--  Made by SecureVote for Swarm
--  Code is available on GitHub and has auditing software
--


gethDialogV : Model -> Html Msg
gethDialogV model =
    div []
        [ Options.styled div [ headline, cs "black" ] [ text "Ballot Transaction:" ]
        , div [ class "mw7 ph3 overflow-visible center" ] [ pre [ class "tl" ] [ text <| candTxText model.candidateTx ] ]
        , Options.styled div [ headline, cs "black" ] [ text "How to send via GETH CLI" ]
        , p [] [ text loremIpsum ]
        , b [] [ text "Example Code Snippet:" ]

        -- Might want to use elm-markdown package for code snippets.
        , div [ class "ba pa3 overflow-scroll nowrap" ]
            [ p [] [ text "geth --datadir ~/.ethereum_private init ~/dev/genesis.json" ]
            , p [] [ text "geth --fast --cache 512 --ipcpath ~/Library/Ethereum/geth.ipc --networkid 1234 --datadir ~/.ethereum_private  console" ]
            ]
        ]


verifyDialogV : Html Msg
verifyDialogV =
    div [] [ text "This is verify ballot dialog box" ]


customDialogV : DialogHtml msg -> Html msg
customDialogV content =
    div [] [ dialogHtmlRender [] content ]


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


loremIpsum : String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur sagittis semper mi, sit amet laoreet mauris lobortis sed. Sed facilisis justo non sagittis sodales. Proin ornare interdum euismod. Cras ultricies ante vitae convallis viverra. Donec dapibus odio ac metus consequat consectetur vel quis massa. Nunc mattis feugiat erat at porta. Praesent varius felis non ullamcorper condimentum. Ut vitae posuere massa. Aenean vitae euismod mauris. Nunc turpis augue, porttitor at massa eget, gravida vehicula nisl."
