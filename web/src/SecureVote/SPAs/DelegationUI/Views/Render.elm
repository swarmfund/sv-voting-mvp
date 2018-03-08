module SecureVote.SPAs.DelegationUI.Views.Render exposing (..)

import Element exposing (Element, column, el, html, paragraph, text)
import Element.Attributes exposing (..)
import Html as Html
import Json.Encode as E
import SecureVote.Components.UI.Code exposing (codeWScroll)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(..), UiElem)


renderTxPreview : Model -> UiElem
renderTxPreview model =
    let
        tx =
            model.delTx

        dlgtTxStr =
            E.encode 4 <|
                E.object
                    [ ( "to", E.string tx.to )
                    , ( "from", E.string tx.from )
                    , ( "value", E.int tx.value )
                    , ( "data", E.string tx.data )
                    ]
    in
    column TxPreview
        [ padding 20 ]
        [ codeWScroll CS dlgtTxStr ]


renderPayload : Model -> UiElem
renderPayload model =
    el PayloadHash [ padding 20 ] (codeWScroll CS model.delTx.data)
