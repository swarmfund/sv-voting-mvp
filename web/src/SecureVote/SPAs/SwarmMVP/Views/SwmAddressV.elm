module SecureVote.SPAs.SwarmMVP.Views.SwmAddressV exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (body1, display1, display2, display3, headline, title)
import Maybe.Extra exposing ((?), isNothing)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Utils exposing (isValidEthAddress, setCandTxFrom)
import SecureVote.SPAs.SwarmMVP.Ballot as Ballot exposing (BallotOption)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getSwmAddress, setSwmAddress, swmAddrId)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(GetErc20Balance))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmHowToVoteR))


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

        setBallotDefaultMsgs =
            List.map .id
                >> List.map (\id_ -> SetBallotRange id_ 0)
            <|
                Ballot.voteOptions

        msgs =
            MultiMsg <|
                [ PageGoForward SwmHowToVoteR
                , SetCandidateTx (setCandTxFrom <| getSwmAddress model ? "AddressView getSwmAddress error")
                , ToWeb3 GetErc20Balance
                ]
                    ++ setBallotDefaultMsgs

        devMsgs =
            MultiMsg <|
                [ PageGoForward SwmHowToVoteR
                , setSwmAddress "0x71c1c1a30f07017f3278333c996ca4e4d71f2092"
                , SetCandidateTx <| setCandTxFrom "0x71c1c1a30f07017f3278333c996ca4e4d71f2092"
                , ToWeb3 GetErc20Balance
                ]
                    ++ setBallotDefaultMsgs

        devBtn =
            btn 394879384 model [ PriBtn, Attr (class "ph2"), Click devMsgs ] [ text "Dev Continue" ]
    in
    fullPageSlide 384938493
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "Your SWM Address" ]
            , Options.styled span [ headline, cs "black db pa2 mv3" ] [ text "Please enter your Swarm address below" ]
            , Options.styled span [ body1, cs "black db pa2 mv3" ] [ text "(Dev) Example address: 0x71c1c1a30f07017f3278333c996ca4e4d71f2092" ]
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
                            , Textf.error addrErrMsg |> Options.when addrErr
                            , css "min-width" "400px"
                            ]
                            []
                        ]
                    ]
                , div [ class "mv3" ]
                    [ btn 894823489 model [ PriBtn, Attr (class "ph2"), Click msgs, btnDisabled ] [ text "Continue" ]
                    , devBtn
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
