module SecureVote.SPAs.DelegationUI.Views.RootV exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Html exposing (Html)
import SecureVote.Components.UI.Collapsible exposing (collapsible)
import SecureVote.Components.UI.CommonStyles exposing (cmnSpacing)
import SecureVote.Components.UI.Typo exposing (pageTitle, subtitle, title)
import SecureVote.SPAs.DelegationUI.Helpers exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(..))
import SecureVote.SPAs.DelegationUI.Views.DelegationForms exposing (..)
import SecureVote.SPAs.DelegationUI.Views.Render exposing (..)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (..)


rootV : Model -> Html Msg
rootV model =
    layout stylesheet <|
        el DNoS [ padding 20, width fill, height fill, maxWidth <| px 800 ] <|
            column DNoS
                [ spacing <| cmnSpacing * 2 ]
                [ pageTitle CS "SV.Light Delegation"
                , collapsible CS
                    { header = "Set Delegation"
                    , onCollapse = ToggleBoolField setDlgtCollapseId
                    , isCollapsed = getBoolFieldWD model setDlgtCollapseId
                    , body =
                        [ delegationFields model
                        , subtitle CS "Delegation Tx Preview"
                        , renderTxPreview model
                        , subtitle CS "Write Delegation To Blockchain"
                        , paragraph DNoS
                            []
                            [ text "Use the 'send with MetaMast' button, or send the transaction above manually from your wallet."
                            ]
                        , setDelegationBtns model
                        ]
                    , startOpen = False
                    }
                , collapsible CS
                    { header = "View Delegation"
                    , onCollapse = ToggleBoolField viewDlgtCollapsedId
                    , isCollapsed = getBoolFieldWD model viewDlgtCollapsedId
                    , body = [ viewDlgtFields model, viewDelegateResp model ]
                    , startOpen = True
                    }
                ]
