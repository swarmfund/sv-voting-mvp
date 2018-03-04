module SecureVote.Ballots.Encoders exposing (..)

import Array
import Json.Encode as E exposing (encode, null, object)
import Maybe exposing (withDefault)
import Maybe.Extra exposing ((?), isJust)
import SecureVote.Ballots.Types exposing (..)


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
            E.object
                [ ( "optionsVersion", E.int 2 )
                , ( "options", E.null )
                ]


bSpecValueToString v =
    encode 4 v
