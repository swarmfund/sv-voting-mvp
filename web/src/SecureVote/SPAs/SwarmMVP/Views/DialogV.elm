module SecureVote.SPAs.SwarmMVP.Views.DialogV exposing (..)

import Html exposing (Html, a, b, div, em, img, li, p, pre, span, text, ul)
import Html.Attributes exposing (class, href, src, target)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (headline, menu, title)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn as Btn exposing (BtnProps(..), btn)
import SecureVote.Eth.Utils exposing (isValidTxid)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml, dialogHtmlRender)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (codeSection, codepointToBinary, getBallotTxid, getEthNodeTemp, setBallotTxid, setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(CheckTxid, SetProvider))
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(..))
import SecureVote.SPAs.SwarmMVP.Views.SwmDelegateV exposing (delegateExplanationCopy)
import SecureVote.SPAs.SwarmMVP.Views.SwmHowToVoteV exposing (combinedHowToVoteCopy)


subhead : String -> Html Msg
subhead s =
    Options.styled div [ title, cs "black db mv3" ] [ text s ]


settingsDialogV : Model -> Html Msg
settingsDialogV model =
    let
        setEthNodeMsgs =
            MultiMsg
                [ SetEthNode <| getEthNodeTemp model ? ""
                , ToWeb3 SetProvider
                ]

        btnDisabled =
            if model.ethNode == getEthNodeTemp model ? "" then
                Disabled
            else
                BtnNop
    in
    div []
        [ div []
            [ Options.styled span [ menu, cs "" ] [ text "Ethereum Node URL" ]
            , Textf.render Mdl
                [ 7785646743 ]
                model.mdl
                [ Options.onInput <| setEthNodeTemp
                , Textf.value <| getEthNodeTemp model ? ""

                -- Seems to be an issue with the label not disappearing properly.
                --                , Textf.label initModel.ethNode
                , cs "mh4"
                , css "min-width" "400px"
                ]
                []
            , btn 456467568 model [ PriBtn, Click <| setEthNodeMsgs, btnDisabled ] [ text "Update" ]
            ]
        ]


infoDialogV : Html Msg
infoDialogV =
    let
        codeSourceSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "About this voting tool" ]
                , text "This voting tool was built by "
                , a [ href "https://secure.vote", target "_blank" ] [ text "SecureVote" ]
                , text " for "
                , a [ href "https://swarm.fund", target "_blank" ] [ text "Swarm Fund" ]
                , text ". The source code can be found at "
                , a [ href "https://github.com/swarmfund/sv-voting-mvp", target "_blank" ] [ text "https://github.com/swarmfund/sv-voting-mvp" ]
                , text ". All votes are validated through an auditing suite. Instructions on how to run your own version of the auditing suite can be found at "
                , a [ href "https://github.com/swarmfund/sv-voting-mvp", target "_blank" ] [ text "https://github.com/swarmfund/sv-voting-mvp" ]
                , div [ class "mt4 tc" ]
                    [ a [ href "https://secure.vote", target "_blank" ]
                        [ img [ src "img/SecureVote.svg", class "w-30" ] [] ]
                    , a [ href "https://swarm.fund", target "_blank" ]
                        [ img [ src "img/SwarmFund.svg", class "w-40 ml4 v-btm" ] [] ]
                    ]
                ]

        delegateExplanationSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "What is Vote Delegation?" ]
                , text <| String.concat delegateExplanationCopy
                ]
    in
    div [] <|
        combinedHowToVoteCopy
            ++ [ delegateExplanationSection
               , codeSourceSection
               ]


gethDialogV : Model -> Html Msg
gethDialogV model =
    let
        txData =
            case model.candidateTx.data of
                Nothing ->
                    "Error! Unable to get transaction data."

                Just data ->
                    data
    in
    div []
        [ subhead "1. Open up your wallet and go to the send transaction screen"
        , p [] [ text "We're going to send a transaction to the voting smart contract with a pre-prepared data payload." ]
        , subhead "2. Enter the voting contract address"
        , p [] [ text "The voting contract address is:", codeSection [ text model.swarmVotingAddress ] ]
        , subhead "3. Enter transaction data"
        , p []
            [ text "Be sure you've set the 'value' you're sending to 0, and then paste in this transaction data."
            , codeSection [ text txData ]
            , text "If you need to select an amount of gas to use, choose "
            , em [] [ text "at least" ]
            , text " 120,000 gas; we recommend 200,000."
            ]
        , subhead "4. Sign and send your transaction"
        , p [] [ text "And you're done! If you'd like to confirm the finer details of your ballot and confirm it was recorded correctly, check out the 'Cast with M.E.W.' page, as well as the 'Verify Ballot' page." ]
        ]


mewDialog : Model -> Html Msg
mewDialog model =
    let
        pubkey =
            case model.keypair of
                Nothing ->
                    "The app has not generated a keypair for you!"

                Just kp ->
                    kp.hexPk
    in
    div []
        [ subhead "1. Go to MyEtherWallet > Contracts"
        , p [] [ text "You'll need to do this yourself." ]
        , subhead "2. Enter the contract address"
        , p [] [ text "The voting contract address is:", codeSection [ text model.swarmVotingAddress ] ]
        , subhead "3. Enter the ABI"
        , p [] [ text "Copy and paste this into the ABI section:", codeSection [ text model.miniVotingAbi ] ]
        , subhead "4. Copy in your encrypted ballot and ephemeral public key"
        , p []
            [ text "Copy in your encrypted ballot (below) into the corresponding field."
            , text "To verify your encrypted ballot, please see the 'Verify Ballot' screen."
            , codeSection [ text <| "0x" ++ model.encBytes ? "-- Error: Can't find your encrypted ballot in the app state!" ]
            , text "You'll also need to copy in your voting public key. (This was generated as a one-time-use key for this ballot only)"
            , codeSection [ text <| "0x" ++ pubkey ]
            ]
        , subhead "5. Sign and send!"
        , p [] [ text "Next, click 'Write', confirm you are sending 0 ether, and make sure the Gas Limit is at least 115,000 or so (200,000 recommended). Once you're satisfied, click 'Generate Transaction', take note of the raw and signed transactions if you like, then click 'Yes I am sure! Make transaction.'" ]
        , subhead "6. You're done!"
        , p []
            [ text "To verify your ballot was recieved as you cast it, wait for the transaction to confirm. " ]
        , p []
            [ text "Next, choose the 'voterToBallotID' method from the dropdown menu. Enter your address in the field provided and click 'READ'." ]
        , p []
            [ text "Make a note of that number. This is your most *recent* ballot ID. (If you've voted more than once only your most recent vote counts)" ]
        , p []
            [ text "From the dropdown, select 'encryptedBallots' and enter your ballot ID. Confirm the answer you get back matches what you put in before." ]
        , p []
            [ text "Do the same for 'associatedPubkeys' to check your public key was recorded properly." ]
        , p []
            [ text "And you're done!" ]
        ]


verifyDialogV : Model -> Html Msg
verifyDialogV model =
    let
        txidFieldVal =
            getBallotTxid model ? ""

        txidErrMsg =
            if "0x" /= String.slice 0 2 txidFieldVal then
                "Txid must start with 0x"
            else if 66 /= String.length txidFieldVal then
                "Incorrect length"
            else if not (isValidTxid txidFieldVal) then
                "Not a valid txid"
            else
                ""

        txidErr =
            txidErrMsg /= "" && txidFieldVal /= ""

        btnDisabled =
            if not txidErr && txidFieldVal /= "" then
                Btn.BtnNop
            else
                Btn.Disabled

        checkStatusTxt =
            case model.txidCheck of
                TxidSuccess ->
                    "Transaction confirmed and recorded correctly!"

                TxidNotMade ->
                    "Waiting for txid..."

                TxidFail msg ->
                    "Fail! " ++ msg

                TxidInProgress ->
                    "Loading..."
    in
    div []
        [ subhead "Check your transaction"
        , text "Paste in your transaction ID to confirm your ballot was cast correct:"
        , div []
            [ Textf.render Mdl
                [ 7658765856 ]
                model.mdl
                [ Options.onInput <| setBallotTxid
                , Textf.label "Ballot Txid"
                , Textf.floatingLabel
                , Textf.value <| getBallotTxid model ? ""
                , Textf.error txidErrMsg |> Options.when txidErr
                , css "min-width" "400px"
                ]
                []
            ]
        , btn 3984938349 model [ PriBtn, Attr (class "ph2"), Click (ToWeb3 <| CheckTxid txidFieldVal), btnDisabled ] [ text "Check Txid" ]
        , div [ class "mt3" ] [ text "Transaction status: ", text checkStatusTxt ]
        ]


customDialogV : DialogHtml msg -> Html msg
customDialogV content =
    div [] [ dialogHtmlRender [] content ]


debugDialogV : Model -> Html Msg
debugDialogV model =
    let
        liE str =
            li [] [ text str ]
    in
    ul [ class "" ]
        [ liE "Add whatever you want here for debug"
        , liE <| toString model.candidateTx
        , liE <| toString model.encBytes
        , liE <| toString <| Maybe.map String.length model.encBytes
        , liE <| toString model.ballotPlaintext
        , liE <| toString <| Maybe.map List.length model.ballotPlaintext
        , liE <| toString <| Maybe.map (List.map codepointToBinary) model.ballotPlaintext
        ]


loremIpsum : String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur sagittis semper mi, sit amet laoreet mauris lobortis sed. Sed facilisis justo non sagittis sodales. Proin ornare interdum euismod. Cras ultricies ante vitae convallis viverra. Donec dapibus odio ac metus consequat consectetur vel quis massa. Nunc mattis feugiat erat at porta. Praesent varius felis non ullamcorper condimentum. Ut vitae posuere massa. Aenean vitae euismod mauris. Nunc turpis augue, porttitor at massa eget, gravida vehicula nisl."
