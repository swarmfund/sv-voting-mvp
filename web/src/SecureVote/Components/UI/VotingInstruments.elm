module SecureVote.Components.UI.VotingInstruments exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import SecureVote.Ballots.Types exposing (..)
import SecureVote.Components.UI.Typo exposing (subhead)
import SecureVote.Components.UI.VotingInstruments.BinaryYesNo exposing (drawBinaryVotingInstrument)
import SecureVote.Components.UI.VotingInstruments.RangeVoting exposing (..)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)


drawVotingInstrument : Model -> OptsOuter -> Int -> String -> List (Html Msg)
drawVotingInstrument model optType id optionTitle =
    case optType of
        OptsSimple v opts ->
            case v of
                RangeVotingPlusMinus3 ->
                    drawRangeVotingInstrument model id optionTitle

        OptsBinary ->
            drawBinaryVotingInstrument model id optionTitle

        OptsNothing ->
            [ subhead "Error: no voting type for this ballot!" ]
