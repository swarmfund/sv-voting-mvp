module SecureVote.SPAs.DelegationUI.Views.DelegationForms exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Input as I
import Json.Encode as E
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(..))
import SecureVote.Components.UI.Code exposing (codeWScroll)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(..), Variations(Disabled), cmnPad, cmnSpacing)
import SecureVote.Components.UI.StatusMsgs exposing (warning)
import SecureVote.Components.UI.Typo exposing (subtitle)
import SecureVote.Eth.Msg exposing (EthMsg(..))
import SecureVote.Eth.Types exposing (zeroAddr)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (radio, select, textInput)
import SecureVote.SPAs.DelegationUI.Helpers exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(..))
import SecureVote.SPAs.DelegationUI.MsgHandlers exposing (encodeDlgtResp)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(..))
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(..), UiElem)
import SecureVote.Tokens.Types exposing (TokenContract(..), tcChoiceToAddr, tcChoiceToStr)
import SecureVote.Utils.Ports exposing (mkCarry)


mkChoice c =
    I.choice c (text <| tcChoiceToStr c)


mkMenu cs =
    I.menu SubMenu [] <| List.map mkChoice cs


tokenSelector model =
    select CS
        { label = I.labelAbove <| el DNoS [ width fill ] (text "Token Contract")
        , with = model.tokenConAddr
        , max = 5
        , options = [ I.errorBelow <| when (selectTokenInvalid model) <| el ErrTxt [] (text "Please Select a Token Contract") ]
        , menu =
            mkMenu
                [ Swarm
                ]
        }


delegationFields : Model -> UiElem
delegationFields model =
    let
        onClickMsg =
            onClick OnFieldUpdate

        isDisabled =
            delAddrInvalid model || (selectTokenInvalid model && model.delType == Just Token)

        optWarn =
            if isDisabled then
                warning CS "Delegation form is not fully valid"
            else
                empty
    in
    column DNoS
        [ spacing 20, width fill ]
        [ delegateAddrField model
        , globalOrTokenChoice model
        , selectTokenContract model
        , optWarn
        ]


delegateAddrField : Model -> UiElem
delegateAddrField model =
    row DNoS
        []
        [ textInput CS
            { onChange = SetStrField setDelegateAddrId
            , value = getStrField model setDelegateAddrId ? ""
            , label = I.labelAbove <| text "Delegate Address"
            , options =
                [ I.errorBelow <| when (delAddrInvalid model) <| el ErrTxt [] (text "Please enter a valid address") ]
            }
        ]


delAddrInvalid : Model -> Bool
delAddrInvalid model =
    let
        value =
            getStrField model setDelegateAddrId ? ""
    in
    not <| isValidEthAddress value


globalOrTokenChoice : Model -> UiElem
globalOrTokenChoice model =
    row DNoS
        []
        [ radio CS
            { onChange = SetDelegationType
            , selected = model.delType
            , label = I.labelAbove (text "Type of Delegation")
            , options = []
            , choices =
                [ I.choice Global (text "Global")
                , I.choice Token (text "Token")
                ]
            }
        ]


selectTokenContract : Model -> UiElem
selectTokenContract model =
    if model.delType == Just Token then
        row DNoS
            [ width fill ]
            [ tokenSelector model
            ]
    else
        empty


selectTokenInvalid : Model -> Bool
selectTokenInvalid model =
    I.selected model.tokenConAddr == Nothing


setDelegationBtns : Model -> UiElem
setDelegationBtns model =
    let
        method =
            if model.delType == Just Token then
                "setTokenDelegation"
            else
                "setGlobalDelegation"

        args =
            setDelegationArgs model

        toWrite =
            { method = method
            , abi = model.delegationABI
            , addr = model.delegationAddr
            , args = args
            }

        onClick_ =
            onClick (MMsg [ Web3 <| WriteViaMM toWrite ])
    in
    column DNoS
        [ spacing cmnSpacing ]
        [ button (CS Field) [ padding 10, onClick_ ] (text "Set Delegation via MetaMask")
        ]


viewDlgtFields : Model -> UiElem
viewDlgtFields model =
    let
        tokenAddrInput =
            if getStrField model getDlgtTypeId == Just "token" then
                --                textInput CS
                --                    { onChange = SetStrField getDelegationTokenAddrId
                --                    , value = getStrField model getDelegationTokenAddrId ? ""
                --                    , label = I.labelAbove (text "Token Address (ERC20)")
                --                    , options = []
                --                    }
                tokenSelector model
            else
                empty
    in
    column DNoS
        [ spacing cmnSpacing ]
        [ textInput CS
            { onChange = SetStrField getDelegationVoterAddrId
            , value = getStrField model getDelegationVoterAddrId ? ""
            , label = I.labelAbove (text "Voter's Address")
            , options = []
            }
        , radio CS
            { onChange = SetStrField getDlgtTypeId
            , selected = getStrField model getDlgtTypeId
            , label = I.labelAbove (text "Type of Delegation")
            , options = []
            , choices =
                [ I.choice "global" (text "Global")
                , I.choice "token" (text "Token")
                ]
            }
        , tokenAddrInput
        , viewDlgtBtn model
        ]


viewDlgtBtn model =
    let
        toRead =
            { abi = model.delegationABI
            , addr = model.delegationAddr
            , args = viewDelegationArgs model
            , method = "resolveDelegation"
            , carry = mkCarry E.null
            }
    in
    button (CS Field) [ padding 10, onClick (MMsg [ ViewDlgtResp Loading, Web3 <| ReadContract toRead ]) ] (text "Show Delegations")


viewDelegateResp model =
    let
        dlgtText r =
            if r.delegatee == zeroAddr then
                "Voter has no delegate."
            else
                "Voter has delegated to: " ++ r.delegatee
    in
    case model.viewDlgtResp of
        NotAsked ->
            empty

        Loading ->
            subtitle CS "Loading..."

        Failure err ->
            warning CS err

        Success resp ->
            column DNoS
                [ spacing cmnSpacing ]
                [ subtitle CS "View Delegation Response"
                , codeWScroll CS <| E.encode 4 << encodeDlgtResp <| resp
                , text <| dlgtText resp
                ]
