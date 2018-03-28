module SecureVote.SPAs.AdminUI.Views.BallotBuilder exposing (..)

import Array
import Date exposing (day, month, year)
import Date.Extra.Core exposing (fromTime, monthToInt)
import Debug exposing (log)
import Dict
import Element exposing (button, column, downloadAs, el, empty, h3, paragraph, row, subheading, text, when)
import Element.Attributes exposing (center, fill, height, maxWidth, padding, paddingTop, percent, spacing, vary, verticalCenter, width, yScrollbar)
import Element.Events exposing (onClick)
import Element.Input as I exposing (disabled)
import Http exposing (encodeUri)
import Json.Encode as E exposing (encode, null, object)
import Maybe exposing (withDefault)
import Maybe.Extra exposing ((?), isJust)
import RemoteData exposing (RemoteData(Failure, Loading, NotAsked, Success))
import Result exposing (toMaybe)
import Result.Extra
import SecureVote.Ballots.Types exposing (BallotSpec(..), BallotSpecChoice(..), OptsChoice(..), OptsOuter(..), SimpleVer(..), bSpecChoiceToStr, getTitle, oChoiceToStr)
import SecureVote.Components.UI.Code exposing (codeWScroll)
import SecureVote.Components.UI.Collapsible exposing (collapsible)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(LinProcElement), Variations(..), cmnPad, cmnSpacing)
import SecureVote.Components.UI.Loading exposing (loadingIndicator)
import SecureVote.Components.UI.Overlay exposing (greyOut)
import SecureVote.Components.UI.StatusMsgs exposing (warning)
import SecureVote.Components.UI.Typo exposing (subhead, subsubtitle, subtitle)
import SecureVote.SPAs.AdminUI.Components.Input exposing (checkbox, genDropSelect, select, textArea, textInput)
import SecureVote.SPAs.AdminUI.Fields exposing (..)
import SecureVote.SPAs.AdminUI.Helpers exposing (getBoolField, getBoolFieldWD, getIntField, getLoadingField, getStrField)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg(..), ToWeb3Msg(WriteViaMM))
import SecureVote.SPAs.AdminUI.Views.Render exposing (renderTxInfo)
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(BallotHash, CS, Field, NoS, SubMenu), UiElem)
import String exposing (toInt)
import String.Extra exposing (decapitalize, replace)


buildBSpecV01 m =
    let
        gwd i d =
            getStrField m i ? d

        ges i =
            getStrField m i ? ""

        gs i =
            getStrField m i

        nSimpOpts =
            getIntField m rangeVoteNumOptsId ? 1

        mkSimpOpt i =
            { optionTitle = ges (genSimpOptTitleId i), optionDesc = gs (genSimpOptDescId i) }

        opts =
            case m.selectOpts |> Maybe.andThen I.selected of
                Just optChoice ->
                    case optChoice of
                        OChBinary ->
                            OptsBinary

                        OChSimpleRange ->
                            OptsSimple RangeVotingPlusMinus3 <| List.map mkSimpOpt (List.range 0 (nSimpOpts - 1))

                Nothing ->
                    OptsBinary
    in
    BVer01
        { ballotTitle = ges bTitleId
        , shortDesc = ges shortDescId
        , longDesc = ges longDescId
        , startTime = (toMaybe <| toInt <| ges startTimeId) ? 0
        , endTime = (toMaybe <| toInt <| ges endTimeId) ? 0
        , erc20Addr = ges erc20Id
        , discussionLink = gs discussId
        , binding = getBoolField m isBindingId ? True
        , encryptionPK = gs encPkId
        , options = opts
        }


ballotBuilder : Model -> UiElem
ballotBuilder model =
    let
        warn =
            [ warning CS "Please Select a Ballot Type" ]

        mShowBallotFields =
            case model.select of
                Nothing ->
                    warn

                Just s ->
                    case I.selected s of
                        Nothing ->
                            warn

                        Just bChoice ->
                            case bChoice of
                                BChoice01 ->
                                    [ buildBallotV1 model ]
    in
    column NoS
        [ spacing 20, height fill ]
    <|
        [ selectBallotType model
        ]
            ++ mShowBallotFields


selectBallotType : Model -> UiElem
selectBallotType model =
    let
        mkChoice c =
            I.choice c (text <| bSpecChoiceToStr c)
    in
    row NoS
        []
        [ select
            { label = I.labelAbove <| text "Select Ballot Type"
            , with = model.select ? genDropSelect
            , max = 5
            , options = []
            , menu =
                I.menu SubMenu
                    []
                    [ mkChoice BChoice01
                    ]
            }
        ]


typicalField model id label =
    textInput
        { onChange = SetStrField id
        , value = getStrField model id ? ""
        , label = I.labelAbove <| text label
        , options = []
        }


buildBallotV1 : Model -> UiElem
buildBallotV1 model =
    column NoS
        [ spacing 20, width fill, maxWidth <| percent 100 ]
    <|
        [ typicalField model democHashId "Democracy ID"
        , buildOpts model
        , typicalField model bTitleId "Ballot Title"
        , textArea
            { onChange = SetStrField shortDescId
            , value = getStrField model shortDescId ? ""
            , label = I.labelAbove <| text "Short Description"
            , options = []
            }
        , textArea
            { onChange = SetStrField longDescId
            , value = getStrField model longDescId ? ""
            , label = I.labelAbove <| text "Long Description"
            , options = []
            }
        , typicalField model startTimeId "Start Time (Epoch Format)"
        , typicalField model endTimeId "End Time (Epoch Format)"
        , typicalField model erc20Id "ERC20 Token Address"
        , typicalField model encPkId "Encryption Public Key (Optional, omit for unencrypted ballot)"
        , typicalField model discussId "Discussion Link (Optional)"
        , checkbox
            { onChange = SetBoolField isBindingId
            , checked = getBoolField model isBindingId ? True
            , label = el NoS [] (text "Vote is Binding")
            , options = []
            }
        ]
            ++ mShowOptFields model
            ++ writeBallot model


buildOpts : Model -> UiElem
buildOpts model =
    let
        mkChoice c =
            I.choice c (text <| oChoiceToStr c)
    in
    column NoS
        [ spacing 20, width fill ]
    <|
        [ row NoS
            []
            [ select
                { label = I.labelAbove <| text "Select Voting Type"
                , with = model.selectOpts ? I.dropMenu Nothing SelectOptType
                , max = 5
                , options = []
                , menu =
                    I.menu SubMenu
                        []
                        [ mkChoice OChSimpleRange
                        , mkChoice OChBinary
                        ]
                }
            ]
        ]


mShowOptFields : Model -> List UiElem
mShowOptFields model =
    let
        warn =
            [ warning CS "Please Select a Voting Type" ]

        ballotOptsHeading =
            subsubtitle CS "Set Ballot Options"
    in
    case model.selectOpts of
        Nothing ->
            warn

        Just s ->
            case I.selected s of
                Nothing ->
                    warn

                Just oCh ->
                    case oCh of
                        OChSimpleRange ->
                            [ ballotOptsHeading, addRangeOpts model ]

                        OChBinary ->
                            [ addBinaryOpts model ]


simpleOptForm model i =
    let
        titleId =
            genSimpOptTitleId i

        descId =
            genSimpOptDescId i

        clpsId =
            "collapse-" ++ titleId
    in
    collapsible CS
        { onCollapse = ToggleBoolField clpsId
        , isCollapsed = getBoolFieldWD model clpsId
        , header = "Option " ++ toString (1 + i)
        , body =
            [ typicalField model titleId "Option Title"
            , typicalField model descId "Option Description (Optional)"
            ]
        , startOpen = True
        , smallTitle = True
        }


addRangeOpts model =
    let
        nOpts =
            getIntField model rangeVoteNumOptsId ? 1

        msgIncOptN =
            SetIntField rangeVoteNumOptsId (nOpts + 1)

        msgDecOptN =
            SetIntField rangeVoteNumOptsId (max 1 <| nOpts - 1)

        pad_ =
            cmnPad / 2
    in
    column NoS [ spacing cmnSpacing ] <|
        [ subtitle CS <| "Number of Options: " ++ toString nOpts ]
            ++ List.map (simpleOptForm model) (List.range 0 (nOpts - 1))
            ++ [ row NoS
                    [ spacing cmnSpacing ]
                    [ when (nOpts > 1) <| button Field [ padding pad_, onClick msgDecOptN ] (text <| "Remove Option " ++ toString nOpts)
                    , button Field [ padding pad_, onClick msgIncOptN ] (text "Add New Option")
                    ]
               ]


genFilename model =
    let
        currDate =
            fromTime <| (*) 1000 <| (Maybe.andThen (toMaybe << toInt) <| getStrField model startTimeId) ? 0

        dateStr =
            String.join "_" <|
                List.map toString
                    [ year currDate, monthToInt <| month currDate, day currDate ]

        bTitleStr =
            decapitalize <| replace " " "-" <| getTitle model.workingBallot
    in
    String.join "-" [ dateStr, bTitleStr, model.hash ] |> (\t -> t ++ ".json")


addBinaryOpts model =
    empty


writeBallot model =
    let
        { indexABI, indexAddr, jsonBallot, hash } =
            model

        democHash =
            getStrField model democHashId |> Result.fromMaybe "Error: No Democracy Hash"

        convTime f =
            (getStrField model f |> Maybe.andThen (toMaybe << toInt)) |> Result.fromMaybe "Error: cannot convert time to integer"

        args =
            Result.map3
                (\dHash sTime eTime ->
                    [ E.string dHash
                    , E.string hash
                    , E.string "0x00"
                    , E.list
                        [ E.int <| sTime
                        , E.int <| eTime
                        ]
                    , E.list
                        [ E.bool <| isJust <| getStrField model encPkId
                        , E.bool False
                        ]
                    ]
                )
                democHash
                (convTime startTimeId)
                (convTime endTimeId)

        ballotToWrite =
            args
                |> Result.map
                    (\args_ ->
                        { abi = indexABI, addr = indexAddr, method = "deployBallot", args = args_ }
                    )

        ballotToRead =
            ballotToWrite
                |> Result.map
                    (\btw ->
                        { btw | args = E.list btw.args }
                    )

        btnRowSpacing =
            5

        wLoading loadingId ( e, eLoading, eDone ) =
            let
                loadingRD =
                    getLoadingField model loadingId ? NotAsked
            in
            row (CS LinProcElement)
                [ paddingTop cmnPad, spacing btnRowSpacing, verticalCenter, maxWidth <| percent 100, width fill ]
                [ loadingIndicator CS (Just loadingRD)
                , case loadingRD of
                    Success _ ->
                        eDone

                    NotAsked ->
                        e

                    Loading ->
                        eLoading

                    Failure err ->
                        column NoS [ spacing cmnSpacing ] [ warning CS err, button Field [ padding 10, onClick <| ResetBallotPubFields "" ] (text "Reset Publishing Process") ]
                ]

        saveJsonBtn btr =
            wLoading saveJsonLoadingIndicator <|
                ( downloadAs { src = "data:application/octet-stream," ++ encodeUri model.jsonBallot, filename = genFilename model } <|
                    button Field
                        [ padding 10
                        , onClick
                            (MMsg
                                [ SetBoolField uploadBallotButtonId True
                                , SetLoadingField saveJsonLoadingIndicator <| Success ""
                                ]
                            )
                        ]
                        (text "Save JSON")
                , text "JSON Saving..."
                , text "JSON Saved!"
                )

        uploadBallotBtn readDoc =
            wLoading uploadBallotLoadingInd <|
                ( button Field
                    [ padding 10
                    , onClick
                        (MMsg
                            [ SetLoadingField uploadBallotLoadingInd Loading
                            , UploadBallot { andThen = SetBoolField showWriteBallotButtonId True }
                            ]
                        )
                    ]
                    (text "Upload Ballot to IPFS and Archive")
                , text "Ballot Uploading"
                , text "Ballot Uploaded"
                )

        writeBallotSection mkBallotTxArgs =
            wLoading writeContractLoadingInd <|
                ( column NoS
                    [ spacing cmnSpacing, maxWidth <| percent 90, width fill ]
                    [ subsubtitle CS "Option 1. Create Ballot Manually"
                    , column NoS
                        [ spacing cmnSpacing, width fill ]
                        [ paragraph NoS [] [ text "Create a transaction in MetaMask (or your chosen wallet) with the following parameters:" ]
                        , row BallotHash [ width fill, padding 10 ] [ codeWScroll CS model.web3.txInfo ]
                        ]
                    , subsubtitle CS "Option 2. Create Ballot using MetaMask"
                    , button Field [ padding 10, onClick (ToWeb3 <| WriteViaMM mkBallotTxArgs) ] (text "Create New Ballot via MetaMask")
                    ]
                , text "Transaction Sending..."
                , text "Transaction Sent!"
                )
    in
    Result.map2
        (\btw btr ->
            column NoS
                [ spacing cmnSpacing, verticalCenter, maxWidth <| percent 100, width fill ]
                [ saveJsonBtn btr
                , when (getBoolField model uploadBallotButtonId ? False) <| uploadBallotBtn btr
                , when (getBoolField model showWriteBallotButtonId ? False) <| writeBallotSection btw
                ]
        )
        ballotToWrite
        ballotToRead
        |> Result.Extra.extract (\err -> warning CS <| "Error while processing ballot: " ++ err)
        |> List.singleton
        |> (::) (subtitle CS "Publish Ballot")
