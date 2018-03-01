module SecureVote.Components.UI.RenderAudit exposing (..)

import Decimal as D
import Dict
import Html exposing (Html, div, li, span, table, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Eth.Types exposing (AuditDoc(..), BallotResult)
import SecureVote.Eth.Utils exposing (isLegacyAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(..))
import Tuple exposing (second)


resultsParas : Model -> Html Msg
resultsParas model =
    div [ class "mb3" ]
        [ div [ class "mb3" ] [ renderAudit model ]
        , btn 893479357 model [ PriBtn, Attr (class "ph2"), Click (SetDialog "Voting Audit Log" FullAuditDialog), OpenDialog ] [ text "Full Voting Audit Log" ]
        ]


resultsEarly : Model -> Html Msg
resultsEarly model =
    let
        btnMsgs =
            MultiMsg [ SetDialog "Voting Audit Log" FullAuditDialog, DoAudit model.currentBallot ]
    in
    div [ class "mb3" ]
        [ btn 43563456 model [ PriBtn, Attr (class "ph2"), Click btnMsgs, OpenDialog ] [ text "See Early Results" ]
        ]


isAuditSuccessMsg : AuditDoc -> Bool
isAuditSuccessMsg msg =
    case msg of
        AuditSuccess _ ->
            True

        _ ->
            False


renderResults : Model -> BallotResult -> Html msg
renderResults model { nVotes, totals } =
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
            ( text t, span [ sCls score ] [ text <| D.toString score ] )

        convHash h =
            if isLegacyAddress model.currentBallot.contractAddr then
                h
                {- We get the titles given to us with the legacy address -}
            else
                Dict.get h model.optHashToTitle ? ""

        titledTotals =
            List.map procHS <|
                List.filter (\( t, s ) -> t /= "") <|
                    List.map (\( h, s ) -> ( convHash h, s )) sortedTotals

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


renderAudit : Model -> Html msg
renderAudit model =
    case List.head <| List.filter isAuditSuccessMsg model.auditMsgs of
        Just (AuditSuccess res) ->
            div [ class "center" ] [ renderResults model res ]

        _ ->
            div [ class "center" ] <| renderAuditLog True model


renderAuditLog : Bool -> Model -> List (Html msg)
renderAuditLog truncate model =
    List.map (renderAuditMsg model) <|
        if truncate then
            List.take 5 model.auditMsgs
        else
            model.auditMsgs


renderAuditMsg : Model -> AuditDoc -> Html msg
renderAuditMsg model auditMsg =
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

        AuditSuccess res ->
            renderResults model res
