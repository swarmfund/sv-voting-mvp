module SecureVote.SPAs.SwarmMVP.Views.SwmVotingV exposing (..)

import Html exposing (Html, code, div, pre, span, text)
import Html.Attributes exposing (class, style)
import Json.Encode exposing (encode)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs)
import Material.Typography exposing (display2, headline)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Encoders exposing (minEthTxEncoder)
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.Eth.Utils exposing (processCandidateTx)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(PageGoForward, SetDialog))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(GethDialog, VerifyDialog), Route(SwmSubmitR))


votingView : Model -> Html Msg
votingView model =
    fullPageSlide 923844759
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
            , div [ class "ba dib pa3 ma3", style [ ( "min-width", "50%" ) ] ]
                [ Options.styled span
                    [ headline, cs "black" ]
                    [ text "Ballot Details:" ]
                , div [] [ text "You selected: Option 1" ]
                ]
            , Options.styled div [ headline, cs "black" ] [ text "Ballot Transaction:" ]
            , div [ class "mw7 ph3 overflow-visible center" ] [ pre [ class "tl" ] [ text <| candTxText model.candidateTx ] ]
            , div [ class "mv4" ]
                [ btn 758678435 model [ SecBtn, Attr (class "ph3") ] [ text "Cast using MEW" ]
                , btn 785784536 model [ SecBtn, Attr (class "ph3"), OpenDialog, Click (SetDialog "Cast using Geth" GethDialog) ] [ text "Cast using Geth" ]
                ]
            , btn 987572349 model [ PriBtn, Attr (class "mv3"), OpenDialog, Click (SetDialog "Verify Your Ballot" VerifyDialog) ] [ text "Verify Ballot" ]
            ]
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
