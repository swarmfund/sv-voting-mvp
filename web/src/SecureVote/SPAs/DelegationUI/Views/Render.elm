module SecureVote.SPAs.DelegationUI.Views.Render exposing (..)

import Element exposing (Element, column, el, text)
import Element.Attributes exposing (fill, height, padding, px, spacing, vary, width)
import Json.Encode as E
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(..), UiElem, Variations(..))


renderTxPreview : Model -> UiElem
renderTxPreview model =
    let
        tx =
            model.delTx

        jsonBallotStr =
            E.encode 4 <|
                E.object
                    [ ( "to", E.string tx.to )
                    , ( "from", E.string tx.from )
                    , ( "value", E.int tx.value )
                    , ( "gas", E.int tx.gas )
                    , ( "data", E.string tx.data )
                    ]
    in
    column TxPreview
        [ padding 20 ]
        [ el TxRender [] (text jsonBallotStr) ]


renderPayload : Model -> UiElem
renderPayload model =
    el PayloadHash [ padding 20, height <| px 60 ] (text model.delTx.data)
