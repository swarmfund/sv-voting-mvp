module SecureVote.SPAs.DelegationUI.Views.RootV exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import SecureVote.Components.UI.Collapsible exposing (collapsible)
import SecureVote.Components.UI.Typo exposing (subtitle, title)
import SecureVote.SPAs.DelegationUI.Helpers exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(..))
import SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (delegationFields)
import SecureVote.SPAs.DelegationUI.Views.Render exposing (renderPayload, renderTxPreview)
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (..)


rootV : Model -> Html Msg
rootV model =
    layout stylesheet <|
        -- An el is the most basic element, like a <div>
        el NoS [ padding 20, width fill, height fill ]
        <|
            column NoS
                [ spacing 20 ]
                [ title CS "SV.Light Delegation UI"
                , collapsible CS
                    { header = "Set Delegation"
                    , onCollapse = ToggleBoolField setDlgtCollapseId
                    , isCollapsed = getBoolFieldWD model setDlgtCollapseId
                    , body = [ delegationFields model ]
                    }
                , collapsible CS
                    { header = "View Delegation"
                    , onCollapse = ToggleBoolField viewDlgtCollapsedId
                    , isCollapsed = getBoolFieldWD model viewDlgtCollapsedId
                    , body =
                        []
                    }
                , collapsible CS
                    { header = "Delegation Tx Preview"
                    , onCollapse = ToggleBoolField txPrevCollapseId
                    , isCollapsed = getBoolFieldWD model txPrevCollapseId
                    , body =
                        [ renderTxPreview model
                        , subtitle CS "Delegation Tx Payload"
                        , renderPayload model
                        ]
                    }
                ]
