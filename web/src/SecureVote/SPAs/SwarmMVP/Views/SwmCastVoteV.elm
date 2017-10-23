module SecureVote.SPAs.SwarmMVP.Views.SwmCastVoteV exposing (..)

import Dict
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Slider as Slider
import Material.Textfield as Textf
import Material.Typography exposing (display2, headline)
import Maybe.Extra exposing ((?), isNothing)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Utils exposing (decimalTo18dps, formatBalance, isValidEthAddress, rawTokenBalance18DpsToBalance, stripTrailingZeros)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (renderReleaseScheduleTitle, voteOptions)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotDisplayMax, ballotDisplayMin, getDelegateAddress, setDelegateAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(BallotDialog), Route(SwmSubmitR))


castVoteView : Model -> Html Msg
castVoteView model =
    let
        swmBalanceStr =
            Maybe.map rawTokenBalance18DpsToBalance model.swmBalance ? "Loading..."

        optionList =
            List.map optionListItem voteOptions

        optionListItem { id, rSchedule, description } =
            let
                rSchedTitle =
                    renderReleaseScheduleTitle rSchedule
            in
            div [ class "center mw-5 cf mb4 mt3 db w-100 bb bw1 b--silver" ]
                [ div [ class "h-100 w-100 w-100-m w-30-l fl mt2 mb3 tl-l v-mid" ]
                    [ span [ class "w-100 f4 tc tl-l v-mid b" ] [ text rSchedTitle ] ]
                , div [ class "cf w-0 w-25-m fl dn dib-m" ]
                    -- &nbsp;
                    [ text " " ]
                , div [ class "cf v-mid w-100 w-50-m w-40-l fl mb2" ]
                    [ div [] [ text <| "Your vote: " ++ toString (Dict.get id model.ballotRange ? 0) ]
                    , div [ class "center" ]
                        [ div [ class "inline-flex flex-row content-center cf relative", style [ ( "top", "-10px" ) ] ]
                            [ span
                                [ class "f3 relative"
                                , style [ ( "top", "0px" ), ( "left", "15px" ) ]
                                ]
                                [ text "👎" ]
                            , div [ class "dib" ]
                                [ Slider.view
                                    [ Slider.value <| toFloat <| Dict.get id model.ballotRange ? 0
                                    , Slider.min <| toFloat ballotDisplayMin
                                    , Slider.max <| toFloat ballotDisplayMax
                                    , Slider.step 1
                                    , Slider.onChange <| SetBallotRange id
                                    , cs ""
                                    ]
                                ]
                            , span
                                [ class "f3 relative"
                                , style [ ( "top", "0px" ), ( "right", "13px" ) ]
                                ]
                                [ text "❤️" ]
                            ]
                        ]
                    ]
                , div [ class "v-mid w-100 w-25-m w-30-l fl mb3 tr-l tr-m tc" ]
                    [ btn (id * 13 + 1)
                        model
                        [ SecBtn
                        , Click (SetDialog (rSchedTitle ++ ": Details") (BallotDialog description))
                        , OpenDialog
                        ]
                        [ text "Details" ]

                    --                        , Icon
                    --                        ]
                    --                        [ MIcon.view "help_outline" [ MIcon.size24 ] ]
                    ]
                ]

        btnDisabled =
            if addrErr || (isNothing <| getDelegateAddress model) then
                Disabled
            else
                BtnNop

        ( addrErr, addrErrMsg ) =
            validAddress model

        progressMsgs =
            MultiMsg
                [ ConstructBallotPlaintext
                , PageGoForward SwmSubmitR
                ]
    in
    fullPageSlide 123413553
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "Swarm Liquidity Vote" ]
            , Options.styled span [ headline, cs "black dib ba pa3 ma3" ] [ text <| "SWM Balance: " ++ swmBalanceStr ]
            , div [ class "mw7 center black" ] optionList
            , p [] [ text "Some text explaining delegation goes here.." ]
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
            , btn 894823489 model [ PriBtn, Attr (class "ma3"), Click progressMsgs, btnDisabled ] [ text "Continue" ]
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
                ( False, "Use Default Address" )
            else
                ( True, "Invalid address" )
