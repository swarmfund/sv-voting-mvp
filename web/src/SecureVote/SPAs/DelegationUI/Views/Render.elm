module SecureVote.SPAs.DelegationUI.Views.Render exposing (..)

import Element exposing (Element, bold, column, el, html, paragraph, row, text)
import Element.Attributes exposing (..)
import Html as Html
import Json.Encode as E
import SecureVote.Components.UI.Code exposing (codeWScroll, codesWScroll)
import SecureVote.Components.UI.CommonStyles exposing (..)
import SecureVote.Components.UI.Typo exposing (subsubtitle)
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


renderTxForMew : Model -> UiElem
renderTxForMew model =
    let
        tx =
            model.delTx
    in
    codesWScroll CS
        [ "Send Tx To: " ++ tx.to ++ "\n"
        , "Value     : " ++ "0" ++ "\n"
        , "Tx Data   : " ++ tx.data
        ]


renderPayload : Model -> UiElem
renderPayload model =
    el PayloadHash [ padding 20 ] (codeWScroll CS model.delTx.data)
