module SecureVote.SPAs.SwarmMVP.Views.SetAddressV exposing (..)

import Dict
import Html exposing (Html, div, p, span, strong, text)
import Html.Attributes exposing (class)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Maybe.Extra exposing ((?), isNothing)
import Monocle.Common exposing (dict)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.Eth.Utils exposing (isValidEthAddress, setCandTxFrom)
import SecureVote.LocalStorage exposing (LsMsg(SetLocalStorage))
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Fields exposing (..)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getUserErc20Addr, setUserErc20Addr)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(GetErc20Balance))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))


swmAddressV : Model -> Html Msg
swmAddressV model =
    let
        ( addrErr, addrErrMsg ) =
            validAddress model

        btnDisabled =
            if addrErr || (isNothing <| getUserErc20Addr model) then
                Disabled
            else
                BtnNop

        msgs isSkip =
            let
                userAddr =
                    getUserErc20Addr model ? ""
            in
            MultiMsg <|
                (if isSkip then
                    [ SetCandidateTx (setCandTxFrom "") ]
                 else
                    [ SetCandidateTx (setCandTxFrom userAddr)
                    , LS <| SetLocalStorage ( lsAddrId, userAddr )
                    , CheckForPrevVotes
                    ]
                )
                    ++ [ PageGoForward ListAllVotesR ]

        devMsgs =
            MultiMsg <|
                [ PageGoForward ListAllVotesR
                , setUserErc20Addr "0x71c1c1a30f07017f3278333c996ca4e4d71f2092"
                , SetCandidateTx <| setCandTxFrom "0x71c1c1a30f07017f3278333c996ca4e4d71f2092"
                ]

        devBtn =
            if model.dev then
                [ btn 394879384 model [ PriBtn, Attr (class "pa2"), Click devMsgs ] [ text "Dev Continue" ] ]
            else
                []

        rememberedAddr =
            case Dict.get lsAddrId model.lsBucket of
                Just addr ->
                    div [ class "mh2" ] [ text <| "Last address used: " ++ addr ]

                Nothing ->
                    span [] []
    in
    fullPageSlide 384938493
        model
        "Your Ethereum Address"
        [ Options.styled span [ cs "dark-gray db pa2 mt3 f4" ] [ text <| "Please enter your Ethereum address holding your tokens below." ]
        , Options.styled p [ cs "" ] [ strong [] [ text "Note: " ], text <| "Your address is used to confirm your token balance and show which ballots you've voted in.", strong [] [ text "This is an optional step." ] ]
        , subhead "Your ballance will be shown as it was when the ballot was started."
        , div [ class "center" ]
            [ div [ class "flex flex-column items-center mh2" ]
                [ div [ class "w-100 flex flex-column items-start" ]
                    [ Textf.render Mdl
                        [ 83543983 ]
                        model.mdl
                        [ Options.onInput <| setUserErc20Addr
                        , Textf.label "Your Ethereum Address"
                        , Textf.floatingLabel
                        , Textf.value <| getUserErc20Addr model ? ""
                        , Textf.error addrErrMsg |> Options.when addrErr
                        , css "max-width" "400px"
                        , cs "w-100 center"
                        ]
                        []
                    ]
                ]
            , div [ class "mv3" ] <|
                [ btn 394893489
                    model
                    [ SecBtn, Attr (class "pa2"), Click (msgs True) ]
                    [ text "Skip" ]
                , btn 894823489
                    model
                    [ PriBtn, Attr (class "pa2"), Click (msgs False), btnDisabled ]
                    [ text "Continue" ]
                ]
                    ++ devBtn
            , rememberedAddr
            ]
        ]


validAddress : Model -> ( Bool, String )
validAddress model =
    let
        swmAddress =
            getUserErc20Addr model
    in
    case swmAddress of
        Nothing ->
            ( False, "Please paste in your Eth address" )

        Just addr ->
            if isValidEthAddress addr then
                ( False, "Address valid!" )
            else
                ( True, "Invalid address" )
