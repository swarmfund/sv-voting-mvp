module SecureVote.Components.UI.VotingInstruments.RangeVoting exposing (..)

import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Material.Options exposing (cs)
import Material.Slider as Slider
import Maybe.Extra exposing ((?))
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotDisplayMax, ballotDisplayMin, genVoteOptId, getUserErc20Addr)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))


drawRangeVotingInstrument : Model -> Int -> String -> List (Html Msg)
drawRangeVotingInstrument model id optionTitle =
    let
        optBtnCs =
            [ class "f3 relative ba br2 b--silver bg-white-20 bg-animate hover-bg-gold ph1 pt2 pointer shadow-1 noselect z-999", style [ ( "padding-bottom", "1px" ), ( "user-select", "none" ) ] ]
    in
    [ div [] [ text <| "Your vote: " ++ toString (Dict.get id model.ballotRange ? 0) ]
    , div [ class "center" ]
        [ div [ class "inline-flex flex-row content-center cf relative", style [ ( "top", "-10px" ) ] ]
            [ span
                ([ style [ ( "top", "-5px" ), ( "left", "15px" ) ]
                 , onClick (ModBallotRange id (max -3 << flip (-) 1))
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
                       , onClick (ModBallotRange id (min 3 << (+) 1))
                       ]
                )
                [ span [ class "relative noselect", style [ ( "top", "1px" ) ] ] [ text "➕️" ] ]
            ]
        ]
    ]
