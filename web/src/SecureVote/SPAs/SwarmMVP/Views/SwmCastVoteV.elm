module SecureVote.SPAs.SwarmMVP.Views.SwmCastVoteV exposing (..)

import Dict
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Slider as Slider
import Material.Typography exposing (display2, headline)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Utils exposing (decimalTo18dps, formatBalance, rawTokenBalance18DpsToBalance, stripTrailingZeros)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (renderReleaseScheduleTitle, voteOptions)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotDisplayMax, ballotDisplayMin)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(PageGoForward, SetBallotRange, SetDialog))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(BallotDialog), Route(SwmSubmitR))


castVoteView : Model -> Html Msg
castVoteView model =
    let
        swmBalanceStr =
            Maybe.map rawTokenBalance18DpsToBalance model.swmBalance ? "Loading..."

        optionList =
            List.map optionListItem voteOptions

        optionListItem { id, rSchedule, description } =
            div [ class "center mw-5 cf mb4 mt3 db w-100 bb bw1 b--silver" ]
                [ div [ class "h-100 w-100 w-100-m w-30-l fl mt2 mb3 tl-l v-mid" ]
                    [ span [ class "w-100 f4 tc tl-l v-mid b" ] [ text <| renderReleaseScheduleTitle rSchedule ] ]
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
                        , Click (SetDialog "Option Details" (BallotDialog description))
                        , OpenDialog
                        ]
                        [ text "Details" ]

                    --                        , Icon
                    --                        ]
                    --                        [ MIcon.view "help_outline" [ MIcon.size24 ] ]
                    ]
                ]
    in
    fullPageSlide 657980946
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
            , Options.styled span [ headline, cs "black dib ba pa3 ma3" ] [ text <| "SWM Balance: " ++ swmBalanceStr ]
            , div [ class "mw7 center black" ] optionList
            , btn 894823489 model [ PriBtn, Attr (class "mv3"), Click (PageGoForward SwmSubmitR) ] [ text "Continue" ]
            ]
        ]
