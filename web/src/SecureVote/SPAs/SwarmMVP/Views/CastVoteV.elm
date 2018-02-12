module SecureVote.SPAs.SwarmMVP.Views.CastVoteV exposing (..)

import Decimal exposing (eq, zero)
import Dict
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Material.Card as Card
import Material.Options as Options exposing (cs, css)
import Material.Slider as Slider
import Maybe exposing (withDefault)
import Maybe.Extra exposing ((?), isJust)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline)
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
            if "" /= (withDefault "" <| getUserErc20Addr model) then
                [ Options.styled div [ cs "mt2 mb4 pa1 f4" ] [ text <| "Vote weighting (" ++ model.currentBallot.erc20Abrv ++ " balance): " ++ balanceStr ] ]
            else
                []

        dMod d f aM =
            case aM of
                Nothing ->
                    Just d

                Just a ->
                    Just (f a)

        optionListItem { id, title, description } =
            let
                optBtnCs =
                    [ class "f3 relative ba br2 b--silver bg-white-20 bg-animate hover-bg-gold ph1 pt2 pointer shadow-1 noselect z-999", style [ ( "padding-bottom", "1px" ), ( "user-select", "none" ) ] ]
            in
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
                                ([ style [ ( "top", "-5px" ), ( "left", "15px" ) ]
                                 , onClick (ModBallotRange id (dMod -1 <| max -3 << flip (-) 1))
                                 ]
                                    ++ optBtnCs
                                )
                                [ span [ class "relative noselect", style [ ( "top", "3px" ) ] ] [ text "➖" ] ]
                            , div [ class "dib z-5" ]
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
                                (optBtnCs
                                    ++ [ style [ ( "top", "-5px" ), ( "right", "13px" ) ]
                                       , onClick (ModBallotRange id (dMod 1 <| min 3 << (+) 1))
                                       ]
                                )
                                [ span [ class "relative noselect", style [ ( "top", "1px" ) ] ] [ text "➕️" ] ]
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
            [ Options.styled div [ cs "pb3" ] [ headline "Choose Your Vote" ] ]
                ++ balanceV
                ++ [ div [ class "mw7 center black" ] optionList
                   , btn 894823489 model [ PriBtn, Attr (class "ma3"), Click progressMsgs ] [ text "Continue" ]
                   ]
        ]
