module SecureVote.SPAs.AdminUI.Views.BallotBuilder exposing (..)

import Array
import Date exposing (day, month, year)
import Date.Extra.Core exposing (fromTime, monthToInt)
import Debug exposing (log)
import Dict
import Element exposing (button, column, downloadAs, el, empty, h3, row, text)
import Element.Attributes exposing (fill, height, padding, spacing, width, yScrollbar)
import Element.Events exposing (onClick)
import Element.Input as I
import Http exposing (encodeUri)
import Json.Encode as E exposing (encode, null, object)
import Maybe exposing (withDefault)
import Maybe.Extra exposing ((?), isJust)
import Result exposing (toMaybe)
import SecureVote.Ballots.Types exposing (BallotSpec(..), BallotSpecChoice(..), OptsChoice(..), OptsOuter(..), SimpleVer(..), bSpecChoiceToStr, getTitle, oChoiceToStr)
import SecureVote.SPAs.AdminUI.Components.Input exposing (checkbox, genDropSelect, select, textArea, textInput)
import SecureVote.SPAs.AdminUI.Fields exposing (..)
import SecureVote.SPAs.AdminUI.Helpers exposing (getBoolField, getStrField)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg(..), ToWeb3Msg(WriteViaMM))
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(Field, NoS, SubMenu), UiElem)
import String exposing (toInt)
import String.Extra exposing (decapitalize, replace)


updateWrap f x =
    MMsg [ f x, UpdateWrkBallot ]


buildBSpecV01 m =
    let
        gwd i d =
            getStrField m i ? d

        ges i =
            getStrField m i ? ""

        gs i =
            getStrField m i
    in
    BVer01
        { ballotTitle = ges bTitleId
        , shortDesc = ges shortDescId
        , longDesc = ges longDescId
        , startTime = toMaybe <| toInt (ges startTimeId)
        , endTime = (toMaybe <| toInt <| ges endTimeId) ? 0
        , erc20Addr = ges erc20Id
        , discussionLink = gs discussId
        , binding = getBoolField m isBindingId ? True
        , encryptionPK = gs encPkId
        , options = OptsBinary
        }


toNullable f m =
    withDefault E.null (Maybe.map f m)


bSpecToJson b =
    case b of
        BVer01 d ->
            E.object
                [ ( "ballotVersion", E.int 1 )
                , ( "ballotInner"
                  , E.object
                        [ ( "ballotTitle", E.string d.ballotTitle )
                        , ( "shortDesc", E.string d.shortDesc )
                        , ( "longDesc", E.string d.longDesc )
                        , ( "startTime", toNullable E.int d.startTime )
                        , ( "endTime", E.int d.endTime )
                        , ( "erc20Addr", E.string d.erc20Addr )
                        , ( "discussionLink", toNullable E.string d.discussionLink )
                        , ( "binding", E.bool d.binding )
                        , ( "encryptionPK", toNullable E.string d.encryptionPK )
                        , ( "options", oSpecToJson d.options )
                        ]
                  )
                ]

        BVerFF ->
            null


oSpecToJson o =
    let
        simpleOToJson opt =
            E.object
                [ ( "optionTitle", E.string opt.optionTitle )
                , ( "optionDesc", toNullable E.string opt.optionDesc )
                ]
    in
    case o of
        OptsNothing ->
            E.object
                [ ( "optionsVersion", E.int 999999999 )
                , ( "options", E.null )
                ]

        OptsSimple RangeVotingPlusMinus3 opts ->
            E.array <| Array.fromList <| List.map simpleOToJson opts

        OptsBinary ->
            E.null


bSpecValueToString v =
    encode 4 v


ballotBuilder : Model -> UiElem
ballotBuilder model =
    let
        mShowBallotFields =
            case model.select of
                Nothing ->
                    []

                Just s ->
                    case I.selected s of
                        Nothing ->
                            []

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


buildBallotV1 : Model -> UiElem
buildBallotV1 model =
    let
        typicalField id label =
            textInput
                { onChange = updateWrap <| SetStrField id
                , value = getStrField model id ? ""
                , label = I.labelAbove <| text label
                , options = []
                }
    in
    column NoS
        [ spacing 20, width fill ]
    <|
        [ typicalField democHashId "Democracy ID"
        , buildOpts model
        , typicalField bTitleId "Ballot Title"
        , textArea
            { onChange = updateWrap <| SetStrField shortDescId
            , value = getStrField model shortDescId ? ""
            , label = I.labelAbove <| text "Short Description"
            , options = []
            }
        , textArea
            { onChange = updateWrap <| SetStrField longDescId
            , value = getStrField model longDescId ? ""
            , label = I.labelAbove <| text "Long Description"
            , options = []
            }
        , typicalField startTimeId "Start Time (Epoch Format)"
        , typicalField endTimeId "End Time (Epoch Format)"
        , typicalField erc20Id "ERC20 Token Address"
        , typicalField encPkId "Encryption Public Key (Optional, omit for unencrypted ballot)"
        , typicalField discussId "Discussion Link (Optional)"
        , checkbox
            { onChange = updateWrap <| SetBoolField isBindingId
            , checked = getBoolField model isBindingId ? True
            , label = el NoS [] (text "Vote is Binding")
            , options = []
            }
        ]
            ++ mShowOptFields model


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
    case model.selectOpts of
        Nothing ->
            []

        Just s ->
            case I.selected s of
                Nothing ->
                    []

                Just oCh ->
                    case oCh of
                        OChSimpleRange ->
                            [ addRangeOpts model ]

                        OChBinary ->
                            [ addBinaryOpts model ]



-- TODO: Support range voting


addRangeOpts model =
    h3 NoS [] (text "TODO - Range Voting not yet complete")


genFilename model =
    let
        currDate =
            fromTime <| (*) 1000 <| (Maybe.andThen (toMaybe << toInt) <| getStrField model startTimeId) ? 0

        dateStr =
            String.join "-" <|
                List.map toString
                    [ year currDate, monthToInt <| month currDate, day currDate ]
    in
    String.join "-" [ dateStr, String.slice 0 18 model.hash, decapitalize <| replace " " "-" <| getTitle model.workingBallot ++ ".json" ]


addBinaryOpts model =
    let
        { indexABI, indexAddr, jsonBallot, hash } =
            model

        democHash =
            getStrField model democHashId ? "Error: No Democracy Hash"

        convTime f =
            (getStrField model f |> Maybe.andThen (toMaybe << toInt)) ? 0

        {-
           `deployBallot` signature:
           function deployBallot(bytes32 democHash, bytes32 specHash, bytes32 extraData,
                                 uint64 startTime, uint64 endTime, bool useEncryption, bool testing)
        -}
        args =
            E.list
                [ E.string democHash
                , E.string hash
                , E.string "0x00"
                , E.int <| convTime startTimeId
                , E.int <| convTime endTimeId
                , E.bool <| isJust <| getStrField model encPkId
                , E.bool False
                ]

        ballotToWrite =
            { abi = indexABI, addr = indexAddr, method = "deployBallot", args = args }
    in
    row NoS
        [ spacing 10 ]
        [ downloadAs { src = "data:application/octet-stream," ++ encodeUri model.jsonBallot, filename = genFilename model } <|
            button Field [ padding 10 ] (text "Save JSON")
        , button Field [ padding 10, onClick (ToWeb3 <| WriteViaMM ballotToWrite) ] (text "Create New Ballot via MetaMask")
        ]
