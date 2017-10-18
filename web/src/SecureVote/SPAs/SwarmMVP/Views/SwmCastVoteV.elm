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
import SecureVote.SPAs.SwarmMVP.Ballot exposing (voteOptions)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotDisplayMax, ballotDisplayMin)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (DialogRoute(BallotDialog), Msg(PageGoForward, SetBallotRange, SetDialog))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


castVoteView : Model -> Html Msg
castVoteView model =
    let
        swmBalanceStr =
            Maybe.map rawTokenBalance18DpsToBalance model.swmBalance ? "Loading..."

        optionList =
            List.map optionListItem voteOptions

        optionListItem { id, title, description, params } =
            div [ class "mw-5 cf mb5 mt3" ]
                [ span [ class " center w-100 w-15-l w-25-m fl f4 tl v-mid mb2" ] [ text title ]
                , div [ class " w-100 w-70-l w-50-m fl mb3 center" ]
                    [ div [] [ text <| "Your vote is: " ++ toString (Dict.get id model.ballotRange ? 0) ]
                    , div [ class "flex flex-row content-center" ]
                        [ span
                            [ class "f3 relative"
                            , style [ ( "top", "3px" ), ( "left", "15px" ) ]
                            ]
                            [ text "👎" ]
                        , Slider.view
                            [ Slider.value <| toFloat <| Dict.get id model.ballotRange ? 0
                            , Slider.min <| toFloat ballotDisplayMin
                            , Slider.max <| toFloat ballotDisplayMax
                            , Slider.step 1
                            , Slider.onChange <| SetBallotRange id
                            , cs ""
                            ]
                        , span
                            [ class "f3 relative"
                            , style [ ( "top", "3px" ), ( "right", "15px" ) ]
                            ]
                            [ text "❤️" ]
                        ]
                    ]
                , div [ class "dtc w-100 w-15-l w-25-m fr" ]
                    [ btn (id * 13 + 1)
                        model
                        [ SecBtn
                        , Click (SetDialog "Option Details" (BallotDialog description))
                        , OpenDialog
                        ]
                        [ text "Show Details" ]
                    ]
                ]
    in
    fullPageSlide 657980946
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
            , Options.styled span [ headline, cs "black dib ba pa3 ma3" ] [ text <| "SWM Balance: " ++ swmBalanceStr ]
            , div [ class "mw7 center" ] optionList
            , btn 894823489 model [ PriBtn, Attr (class "mv3"), Click (PageGoForward SwmSubmitR) ] [ text "Continue" ]
            ]
        ]
