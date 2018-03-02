module SecureVote.SPAs.DelegationUI.Views.DelegationBuilder exposing (..)

import Array
import Debug exposing (log)
import Dict
import Element exposing (button, column, downloadAs, el, empty, h3, row, text)
import Element.Attributes exposing (fill, height, padding, spacing, width, yScrollbar)
import Element.Events exposing (onClick)
import Element.Input as I
import Http exposing (encodeUri)
import Json.Encode as E exposing (encode, null, object)
import Maybe exposing (withDefault)
import Maybe.Extra exposing ((?))
import Result exposing (toMaybe)
import SecureVote.Ballots.Types exposing (BallotSpec(..), BallotSpecChoice(..), OptsChoice(..), OptsOuter(..), SimpleVer(..), bSpecChoiceToStr, getTitle, oChoiceToStr)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (checkbox, genDropSelect, select, textArea, textInput)
import SecureVote.SPAs.DelegationUI.Helpers exposing (getBoolField, getStrField)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(..))
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (AdminStyles(Field, NoS, SubMenu), UiElem)
import String exposing (toInt)
import String.Extra exposing (decapitalize, replace)


democHashId =
    "demoHashId"


delegateId =
    "delegateId"


selectBallotId : String
selectBallotId =
    "selectBallotElementId"


selectOptionId =
    "selectOptionTypeId"


idB1F s =
    "bBallotV01" ++ s


bTitleId =
    idB1F "Title"


shortDescId =
    idB1F "ShortDesc"


longDescId =
    idB1F "LongDesc"


startTimeId =
    idB1F "StartTime"


endTimeId =
    idB1F "EndTime"


erc20Id =
    idB1F "Erc20"


encPkId =
    idB1F "EncPK"


discussId =
    idB1F "DiscussionLink"


isBindingId =
    idB1F "IsBinding"


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


delegationBuilder : Model -> UiElem
delegationBuilder model =
    --    let
    --        mShowBallotFields =
    --            case model.select of
    --                Nothing ->
    --                    []
    --
    --                Just s ->
    --                    case I.selected s of
    --                        Nothing ->
    --                            []
    --
    --                        Just bChoice ->
    --                            case bChoice of
    --                                BChoice01 ->
    --                                    [ buildBallotV1 model ]
    --    in
    column NoS
        [ spacing 20, height fill ]
    <|
        [ buildBallotV1 model
        ]



--            ++ mShowBallotFields


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
        [ typicalField delegateId "Delegate ID"

        --        , buildOpts model
        --        , typicalField bTitleId "BallotTitle"
        --        , textArea
        --            { onChange = updateWrap <| SetStrField shortDescId
        --            , value = getStrField model shortDescId ? ""
        --            , label = I.labelAbove <| text "Short Description"
        --            , options = []
        --            }
        --        , textArea
        --            { onChange = updateWrap <| SetStrField longDescId
        --            , value = getStrField model longDescId ? ""
        --            , label = I.labelAbove <| text "Long Description"
        --            , options = []
        --            }
        --        , typicalField startTimeId "Start Time (Optional, Epoch Format)"
        --        , typicalField endTimeId "End Time (Epoch Format)"
        --        , typicalField erc20Id "ERC20 Token Address"
        --        , typicalField encPkId "Encryption Public Key"
        --        , typicalField discussId "Discussion Link (Optional)"
        --        , checkbox
        --            { onChange = updateWrap <| SetBoolField isBindingId
        --            , checked = getBoolField model isBindingId ? True
        --            , label = el NoS [] (text "Vote is Binding")
        --            , options = []
        --            }
        ]



--            ++ mShowOptFields model


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
    decapitalize <| replace " " "-" <| getTitle model.workingBallot ++ ".json"


addBinaryOpts model =
    row NoS
        []
        [ downloadAs { src = "data:application/octet-stream," ++ encodeUri model.jsonBallot, filename = genFilename model } <|
            button Field [ padding 10 ] (text "Save JSON")
        , button Field [ padding 10 ] (text "Create New Ballot via MetaMask (Not yet active...)")
        ]
