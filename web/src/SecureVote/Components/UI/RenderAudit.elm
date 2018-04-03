module SecureVote.Components.UI.RenderAudit exposing (..)

import Decimal as D
import Dict
import Html exposing (Html, div, li, span, table, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class)
import Maybe.Extra exposing ((?))
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.Types exposing (..)
import SecureVote.Eth.Types exposing (AuditDoc(..), BallotResult)
import SecureVote.Eth.Utils exposing (formatBalance, isLegacyAddress)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import Tuple exposing (second)


isAuditSuccessMsg : AuditDoc -> Bool
isAuditSuccessMsg msg =
    case msg of
        AuditSuccess _ ->
            True

        _ ->
            False


renderResults : Model -> ( String, BallotSpec ) -> BallotResult -> Html msg
renderResults model ( bHash, bSpec ) { nVotes, totals } =
    let
        sortedTotals =
            List.reverse <| List.sortBy (D.toFloat << second) totals

        sCls s =
            if D.lt s D.zero then
                class "tr red"
            else
                class "tr black"

        {- process (title, score) tuples -}
        procHS ( t, score ) =
            ( text t, span [ sCls score ] [ text <| formatBalance <| D.toString score ] )

        --optTitles =
        --    case bVoteOpts.get bSpec of
        --        OptsSimple RangeVotingPlusMinus3 opts ->
        --            List.map .optionTitle opts
        --
        --        OptsBinary -> ["Resolution: "]
        --
        --        OptsNothing -> ["Error: Options Not Found!"]
        --
        titledTotals =
            List.map procHS sortedTotals

        commonCs =
            class "pa3"

        trCs =
            class "striped--near-white"

        thCs =
            class "black"

        {- process (title, score) tuples (note: they are `Html msg`s) -}
        procTS ( t, s ) =
            [ td [ commonCs, class "tl" ] [ t ], td [ commonCs, class "tr" ] [ s ] ]
    in
    table [ class "ma3 collapse w-70 center" ] <|
        [ thead [ trCs ] [ th [ commonCs, thCs ] [ text "Option" ], th [ commonCs, thCs ] [ text "Votes" ] ] ]
            ++ List.map (tr [ trCs ] << procTS) titledTotals


renderAudit : Model -> ( String, BallotSpec ) -> Html msg
renderAudit model currBallot =
    case List.head <| List.filter isAuditSuccessMsg model.auditMsgs of
        Just (AuditSuccess res) ->
            div [ class "center" ] [ renderResults model currBallot res ]

        _ ->
            div [ class "center" ] <| renderAuditLog True model currBallot


renderAuditLog : Bool -> Model -> ( String, BallotSpec ) -> List (Html msg)
renderAuditLog truncate model currBallot =
    List.map (renderAuditMsg model currBallot) <|
        if truncate then
            List.take 5 model.auditMsgs
        else
            model.auditMsgs


renderAuditMsg : Model -> ( String, BallotSpec ) -> AuditDoc -> Html msg
renderAuditMsg model currBallot auditMsg =
    let
        wrapper attrs msg =
            span ([ class "db tl pa1 bb bb-silver" ] ++ attrs) [ text msg ]
    in
    case auditMsg of
        AuditLog msg ->
            wrapper [] msg

        AuditLogErr msg ->
            wrapper [ class "red" ] msg

        AuditFail msg ->
            wrapper [ class "red bold" ] msg

        AuditWarn msg ->
            wrapper [ class "orange" ] msg

        AuditSuccess res ->
            renderResults model currBallot res
