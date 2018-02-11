module SecureVote.SPAs.SwarmMVP.Views.CastVoteV exposing (..)

import Decimal exposing (eq, zero)
import Dict
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Material.Slider as Slider
import Material.Typography exposing (display2, headline)
import Maybe.Extra exposing ((?), isJust)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Utils exposing (rawTokenBalance18DpsToBalance)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotDisplayMax, ballotDisplayMin, getUserErc20Addr)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(BallotDialog), Route(SwmDelegateR))


castVoteView : Model -> Html Msg
castVoteView model =
    let
        balanceStr =
            let
                userErc20AddrM =
                    getUserErc20Addr model

                balM =
                    model.currentBallot.erc20Balance

                ifZeroOr addr b =
                    if eq b zero then
                        "No tokens for " ++ addr ++ " in ERC20 contract at " ++ model.currentBallot.erc20Addr
                    else
                        rawTokenBalance18DpsToBalance b
            in
            Maybe.map2 ifZeroOr userErc20AddrM balM ? "Loading..."

        optionList =
            List.map optionListItem model.currentBallot.voteOptions

        balanceV =
            if isJust <| getUserErc20Addr model then
                [ Options.styled span [ headline, cs "black dib ba pa3 ma3" ] [ text <| model.currentBallot.erc20Abrv ++ " Balance: " ++ balanceStr ] ]
            else
                []

        optionListItem { id, title, description } =
            div [ class "center mw-5 cf mb4 mt3 db w-100 bb bw1 b--silver" ]
                [ div [ class "h-100 w-100 w-100-m w-30-l fl mt2 mb3 tl-l v-mid" ]
                    [ span [ class "w-100 f4 tc tl-l v-mid b" ] [ text title ] ]
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
                        , Click (SetDialog (title ++ ": Details") (BallotDialog description))
                        , OpenDialog
                        ]
                        [ text "Details" ]

                    --                        , Icon
                    --                        ]
                    --                        [ MIcon.view "help_outline" [ MIcon.size24 ] ]
                    ]
                ]

        progressMsgs =
            MultiMsg
                [ ConstructBallotPlaintext
                , PageGoForward SwmDelegateR
                ]
    in
    fullPageSlide 123413553
        model
        []
        [ Card.text [ cs "center tc" ] <|
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "Choose Your Vote" ] ]
                ++ balanceV
                ++ [ div [ class "mw7 center black" ] optionList
                   , btn 894823489 model [ PriBtn, Attr (class "ma3"), Click progressMsgs ] [ text "Continue" ]
                   ]
        ]
