module SecureVote.SPAs.SwarmMVP.Views.VotingV exposing (..)

import Dict
import Html exposing (Html, b, br, code, div, li, p, pre, span, table, td, text, tr, ul)
import Html.Attributes exposing (class, id, style)
import Json.Encode exposing (encode)
import Material.Card as Card
import Material.Options as Options exposing (cs)
import Material.Typography as Typo exposing (display2, title)
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(Success))
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.Types exposing (..)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Loading exposing (loadingSpinner)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.Components.UI.VotingInstruments.BinaryYesNo exposing (yesNoToString)
import SecureVote.Crypto.Hashing exposing (hashToInt)
import SecureVote.Eth.Encoders exposing (minEthTxEncoder)
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.Eth.Utils exposing (processCandidateTx)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (formatTsAsDate, genVoteOptId, getDelegateAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(GethDialog, MEWDialog, VerifyDialog), Route(SwmSubmitR))
import SecureVote.Utils.Lists exposing (enumerate)


votingView : Model -> ( String, BallotSpec ) -> Html Msg
votingView model ( bHash, bSpec ) =
    let
        tableRow ( desc, value ) =
            tr []
                [ td [ class "b tr" ] [ text <| desc ++ ": " ]
                , td [ class "tl", style [ ( "word-wrap", "break-word" ) ] ] [ text value ]
                ]

        getResults ( i, { optionTitle, optionDesc } ) =
            ( optionTitle
            , toString <| Dict.get (genVoteOptId bHash i) model.ballotRange ? 0
            )

        renderBinaryVote =
            -- show the use an error if something broke
            yesNoToString <| Dict.get (genVoteOptId bHash 0) model.ballotRange ? -999

        drawOptSummary opts =
            case opts of
                OptsSimple simpleVer simpleOpts ->
                    List.map tableRow <| List.map getResults (enumerate simpleOpts)

                OptsBinary ->
                    [ tableRow ( "Your Vote", renderBinaryVote ) ]

                OptsNothing ->
                    [ tableRow ( "Error", "No Options Available" ) ]

        displayVoteSummary =
            table [ class "mt2 dt--fixed w-auto-l center" ] <|
                Maybe.map drawOptSummary (bVoteOpts.getOption bSpec)
                    ? []

        ( startTime, endTime ) =
            case ( bStartTime.getOption bSpec, bEndTime.getOption bSpec ) of
                ( Just startTime, Just endTime ) ->
                    ( startTime, endTime + (15 * 60) )

                _ ->
                    ( 0xFFFFFFFF, 0xFFFFEEEE )

        ballotClosedWarning =
            if startTime > model.now then
                Options.styled div [ Typo.headline, cs "red mv3" ] [ text <| "Warning! Ballot not yet open! Opens on " ++ formatTsAsDate startTime ]
            else
                div [] []

        voteBtnsOrTxid model =
            case model.metamaskTxid of
                Nothing ->
                    div [ class "mv2" ]
                        [ btn 758678435 model [ SecBtn, Attr (class "pa2"), Click (SetDialog "Cast your vote using MyEtherWallet" MEWDialog), OpenDialog ] [ text "Cast using M.E.W." ]
                        , btn 483974399 model [ SecBtn, Attr (class "pa2"), Click VoteWMetaMask ] [ text "Cast using MetaMask" ]
                        , btn 785784536 model [ SecBtn, Attr (class "pa2"), Click (SetDialog "Cast using Other" GethDialog), OpenDialog ] [ text "Cast using Other" ]
                        ]

                Just txid ->
                    div [ class "mv2 f5 w-100 overflow-x-hidden" ] [ text "Vote cast using MetaMask.", br [] [], text <| "TXID: " ++ txid ]

        endBtns model =
            div []
                [ btn 987572349 model [ PriBtn, Attr (class "ma2"), Click (SetDialog "Verify Your Ballot" VerifyDialog), OpenDialog ] [ text "Verify Ballot" ]
                , btn 843973394 model [ SecBtn, Attr (class "ma2"), Click PageGoHome ] [ text "Back to Start" ]
                ]

        ballotDetails =
            div []
                [ subhead "Ballot Transaction:"
                , div [ class "mw7 center" ] [ pre [ class "tl" ] [ text <| candTxText model.candidateTx ] ]
                , ballotClosedWarning
                , voteBtnsOrTxid model
                , span [ class "" ] [ subhead <| "Results available " ++ formatTsAsDate endTime ]
                , endBtns model
                ]

        ballotDetailsSection =
            if model.ballotAllDone then
                ballotDetails
            else
                loadingSpinner "Waiting for ballot encryption..."
    in
    fullPageSlide 923844759
        model
        "Cast Your Vote"
        [ div [ class "ba dib pa2 ma1", style [ ( "min-width", "50%" ) ] ]
            [ subhead "Your Vote Summary:"
            , displayVoteSummary
            ]
        , ballotDetailsSection
        ]


candTxText : CandidateEthTx -> String
candTxText candTx =
    let
        minTx =
            processCandidateTx candTx
    in
    case minTx of
        Nothing ->
            "Error generating Tx details"

        Just rec ->
            encode 2 <| minEthTxEncoder rec



--generateMewUri : Model -> String
--generateMewUri model =
--    let
--        candTx =
--            model.candidateTx
--
--        baseUrl =
--            "https://www.myetherwallet.com/?"
--
--        from =
--            "from=" ++ candTx.from ? ""
--
--        to =
--            "&to=" ++ candTx.to ? ""
--
--        value =
--            "&value=" ++ toString candTx.value
--
--        gas =
--            "&gas=" ++ candTx.gas
--
--        data =
--            "&data=" ++ candTx.data ? ""
--
--        tail =
--            "#send-transaction"
--    in
--    baseUrl ++ from ++ to ++ value ++ data ++ tail
