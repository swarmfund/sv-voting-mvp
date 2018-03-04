module SecureVote.Components.UI.VotingInstruments.BinaryYesNo exposing (..)

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


yesNoToString : Int -> String
yesNoToString i =
    case i of
        1 ->
            "Yes"

        (-1) ->
            "No"

        0 ->
            "Choose an option"

        _ ->
            "Error: vote out of bounds"


drawBinaryVotingInstrument : Model -> Int -> String -> List (Html Msg)
drawBinaryVotingInstrument model id optionTitle =
    let
        optBtnCs =
            [ class "mv1 mh3 pa3 f3 relative ba br2 b--silver bg-white-20 bg-animate hover-bg-gold ph1 pt2 pointer shadow-1 noselect z-999", style [ ( "padding-bottom", "1px" ), ( "user-select", "none" ) ] ]
    in
    [ div [] [ text <| "Your vote: " ++ yesNoToString (Dict.get id model.ballotRange ? 0) ]
    , div [ class "center" ]
        [ div [ class "inline-flex flex-row content-center cf relative" ]
            [ span
                ([ onClick (SetBallotRange id -1)
                 ]
                    ++ optBtnCs
                )
                [ div [ class "noselect pa3", style [] ] [ text "No" ] ]
            , span
                (optBtnCs
                    ++ [ onClick (SetBallotRange id 1)
                       ]
                )
                [ div [ class "noselect pa3", style [] ] [ text "Yes️" ] ]
            ]
        ]
    ]
