module SecureVote.SPAs.SwarmMVP.Views.DialogV exposing (..)

import Dict
import Html exposing (Html, a, b, div, em, img, li, p, pre, span, text, ul)
import Html.Attributes exposing (class, href, src, target)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography as Typo exposing (menu, title)
import Maybe.Extra exposing ((?))
import Monocle.Common exposing (dict)
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.Types exposing (..)
import SecureVote.Components.UI.Btn as Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.RenderAudit exposing (renderAuditLog)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.Eth.Utils exposing (isValidTxid)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml, dialogHtmlRender)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (codeSection, codepointToBinary, defaultDelegate, genVoteOptId, getBallotTxid, getDelegateAddress, getEthNodeTemp, setBallotTxid, setEthNodeTemp, toStrDropQts)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(CheckTxid, SetProvider))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(ListAllVotesR))
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(..))
import SecureVote.SPAs.SwarmMVP.Views.HowToVoteV exposing (combinedHowToVoteCopy)
import SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (orderedBallotBits)
import SecureVote.Types.VBit exposing (vBitsToInt, vblToList)
import SecureVote.Utils.Lenses exposing ((=|>))
import SecureVote.Utils.Lists exposing (enumerate)


subsubhead : String -> Html Msg
subsubhead s =
    Options.styled div [ Typo.subhead, cs "black db mv3" ] [ text s ]


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
        [ div [ class "w-100" ]
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
        , div [] [ text "Note: Ethereum nodes require ", em [] [ text "full" ], text " historical access for auditing past ballots. Auditing results will fail if they're not syncing in archive mode." ]
        ]


delegateExplanationCopy : String -> Model -> List String
delegateExplanationCopy bHash model =
    let
        token =
            (mErc20Abrv bHash).get model
    in
    [ "You may optionally select a delegate, though this isn't required. If you would like to, please enter their " ++ token ++ " token address below. "
    , "If your delegate casts a vote, your votes will be replaced with the votes that your delegate chooses. "
    , "If your delegate does not cast a vote, your vote will be cast with the options you have selected. "
    ]


infoDialogV : Model -> Html Msg
infoDialogV model =
    let
        codeSourceSection =
            div []
                [ subhead "About This Voting Tool"
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

        bHash =
            model.currentBallot ? "NO CURRENT BALLOT"

        delegateExplanationSection =
            div []
                [ subhead "What Is Vote Delegation?"
                , text <| String.concat <| delegateExplanationCopy bHash model
                ]

        ballotInfo =
            case model.route of
                ListAllVotesR ->
                    span [] []

                _ ->
                    div []
                        [ subhead "Smart Contract Information"
                        , p [] [ text "The address of the smart contract for this ballot is: ", codeSection [ text <| mCurrVotingAddr.get model ] ]
                        ]

        errLog =
            if List.isEmpty model.errors then
                []
            else
                [ subhead "Error Log"
                , ul [] <|
                    List.map (\e -> li [] [ text e ]) model.errors
                ]
    in
    div [] <|
        combinedHowToVoteCopy bHash model
            ++ [ delegateExplanationSection
               , ballotInfo
               , codeSourceSection
               ]
            ++ errLog


gethDialogV : Model -> ( String, BallotSpec ) -> Html Msg
gethDialogV model ( bHash, bSpec ) =
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
        , p [] [ text "The voting contract address is:", codeSection [ text <| mCurrVotingAddr.get model ] ]
        , subhead "3. Enter transaction data"
        , p []
            [ text "Be sure you've set the 'value' you're sending to 0, and then paste in this transaction data."
            , codeSection [ text txData ]
            , text "If you need to select an amount of gas to use, choose "
            , em [] [ text "at least" ]
            , text " 120,000 gas; we recommend 200,000."
            ]
        , subhead "4. Sign and send your transaction"
        , p [] [ text "Woo! Wait for your transaction to confirm and your ballot should be submitted!" ]
        , subhead "5. Check your transaction"
        , checkTxComponent model
        , subsubhead "Manual check"
        , p [] [ text "If you'd like to confirm the finer details of your ballot and confirm it was recorded correctly, check out the 'Cast with M.E.W.' page, as well as the 'Verify Ballot' page." ]
        ]


mewDialog : Model -> ( String, BallotSpec ) -> Html Msg
mewDialog model ( bHash, bSpec ) =
    let
        ( ( useEnc, encPK ), pubkey ) =
            case ( bEncPK.getOption bSpec, model.keypair ) of
                ( Just encPK, Just kp ) ->
                    ( ( True, encPK ), kp.hexPk )

                _ ->
                    ( ( False, "No Encryption PK" ), "No PK Needed" )

        methodName =
            if useEnc then
                "submitBallotWithPk"
            else
                "submitBallotNoPk"

        copyBallotHeadingExtr =
            if useEnc then
                "and ephemeral public key"
            else
                ""

        ballotCopyExtra =
            if useEnc then
                [ codeSection [ text <| "0x" ++ model.encBytes ? "-- Error: Can't find your encrypted ballot in the app state!" ]
                , text "You'll also need to copy in your voting public key. (This was generated as a one-time-use key for this session only)"
                , codeSection [ text <| "0x" ++ pubkey ]
                ]
            else
                [ codeSection [ text <| "0x" ++ Maybe.withDefault "-- Error: Ballot Plaintext not found!" model.ballotRawHex ]
                ]

        manualVerificationExtra =
            if useEnc then
                [ text "Do the same for 'associatedPubkeys' to check your public key was recorded properly." ]
            else
                []
    in
    div []
        [ subhead "1. Go to MyEtherWallet > Contracts"
        , p [] [ text "You'll need to do this yourself." ]
        , subhead "2. Enter the contract address"
        , p [] [ text "The voting contract address is:", codeSection [ text <| mCurrVotingAddr.get model ] ]
        , subhead "3. Enter the ABI"
        , p [] [ text "Copy and paste this into the ABI section:", codeSection [ text model.ballotBoxABI ] ]
        , subhead <| "4. Copy in your ballot" ++ copyBallotHeadingExtr
        , p [] <|
            [ text <| "Select the '" ++ methodName ++ "' contract method."
            , text "Copy in your ballot (below) into the corresponding field."
            , text "To verify your ballot is exactly as intended, please see the 'Verify Ballot' screen."
            ]
                ++ ballotCopyExtra
        , subhead "5. Sign and send!"
        , p [] [ text "Next, click 'Write', confirm you are sending 0 ether, and make sure the Gas Limit is at least 115,000 or so (200,000 recommended). Once you're satisfied, click 'Generate Transaction', take note of the raw and signed transactions if you like, then click 'Yes I am sure! Make transaction.'" ]
        , subhead "6. You're done!"
        , p []
            [ text "To verify your ballot was recieved as you cast it, wait for the transaction to confirm. " ]
        , subsubhead "Manual verification"
        , p []
            [ text "Next, choose the 'voterToBallotID' method from the dropdown menu. Enter your address in the field provided and click 'READ'." ]
        , p []
            [ text "Make a note of that number. This is your most *recent* ballot ID. (If you've voted more than once only your most recent vote counts)" ]
        , p [] <|
            [ text "From the dropdown, select 'ballotMap' and enter your ballot ID. Confirm the answer you get back matches what you put in before." ]
                ++ manualVerificationExtra
        , p []
            [ text "And you're done!" ]
        , subsubhead "Automatic verification"
        , checkTxComponent model
        ]


verifyDialogV : Model -> ( String, BallotSpec ) -> Html Msg
verifyDialogV model ( bHash, bSpec ) =
    let
        verificationUrl =
            "https://github.com/swarmfund/sv-voting-mvp/blob/master/BallotVerification.md"

        rawVotesMapOver =
            case bVoteOpts.getOption bSpec ? OptsNothing of
                OptsBinary ->
                    [ 0 ]

                OptsSimple RangeVotingPlusMinus3 opts ->
                    List.range 0 (List.length opts)

                OptsNothing ->
                    []

        verificationVars =
            -- we apply toString to these strings so they're wrapped in quotes
            [ ( "ballotEncPk", toString <| model.remoteHexPk ? "err: not found" )
            , ( "myPubkey", toString <| Maybe.map .hexPk model.keypair ? "pubkey not found" )
            , ( "mySeckey", toString <| Maybe.map .hexSk model.keypair ? "seckey not found" )
            , ( "myDelegate", toString <| getDelegateAddress model ? defaultDelegate )
            , ( "myVotesRaw", toString <| List.map (\i -> Dict.get (genVoteOptId bHash i) model.ballotRange ? -9999) rawVotesMapOver )
            , ( "myVotesOffset", toString <| List.map (vBitsToInt << vblToList) <| Result.withDefault [] <| orderedBallotBits ( bHash, bSpec ) model.ballotBits )
            , ( "encBallot", toString <| model.encBytes ? "encrypted ballot not found" )
            , ( "submitBallotPrefix", toString "13c04769" )
            , ( "txData", toString <| model.candidateTx.data ? "tx data not found" )
            , ( "votingContract", toString <| mCurrVotingAddr.get model )
            ]

        renderVerVar ( name, content ) =
            text <| "var " ++ name ++ " = " ++ content ++ ";\n"

        renderedVerVars =
            codeSection <| List.map renderVerVar verificationVars
    in
    div
        []
        [ subhead "Check your transaction"
        , checkTxComponent model
        , subhead "Verify you ballot was constructed and encrypted correctly"
        , p [] [ text "For this, you'll need to copy down these variables, and follow the instructions at ", a [ href verificationUrl, target "_blank" ] [ text verificationUrl ] ]
        , renderedVerVars
        ]


checkTxComponent : Model -> Html Msg
checkTxComponent model =
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
                    span [ class "green" ] [ text "Transaction confirmed and recorded correctly!" ]

                TxidNotMade ->
                    text "Waiting for txid..."

                TxidFail msg ->
                    span [ class "red" ] [ text <| "Fail! " ++ msg ]

                TxidInProgress ->
                    text "Loading..."
    in
    div []
        [ text "Paste in your transaction ID to confirm your ballot was cast correctly:"
        , div []
            [ Textf.render Mdl
                [ 7658765856 ]
                model.mdl
                [ Options.onInput <| setBallotTxid
                , Textf.label "Ballot Txid"
                , Textf.floatingLabel
                , Textf.value <| getBallotTxid model ? ""
                , Textf.error txidErrMsg |> Options.when txidErr
                , css "max-width" "400px"
                , cs "db w-100"
                ]
                []
            ]
        , btn 3984938349 model [ PriBtn, Attr (class "ph2"), Click (ToWeb3 <| CheckTxid txidFieldVal), btnDisabled ] [ text "Check Txid" ]
        , div [ class "mt3" ] [ text "Transaction status: ", checkStatusTxt ]
        ]


customDialogV : DialogHtml msg -> Html msg
customDialogV content =
    div [] [ dialogHtmlRender [] content ]


fullAuditDialogV : Model -> ( String, BallotSpec ) -> Html msg
fullAuditDialogV model ( bHash, bSpec ) =
    div [] <| renderAuditLog False model ( bHash, bSpec )
