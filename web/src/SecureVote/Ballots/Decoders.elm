module SecureVote.Ballots.Decoders exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import SecureVote.Ballots.Types exposing (..)


handleBSpec : Int -> Value -> Result String BallotSpec
handleBSpec v val =
    case v of
        1 ->
            handleBV01Spec val |> Result.map BVer01

        _ ->
            Err "Unknown BallotSpec Version"


handleBV01Spec : Value -> Result String BSpec01Impl
handleBV01Spec val =
    let
        decoder =
            decode BSpec01Impl
                |> required "ballotTitle" string
                |> required "shortDesc" string
                |> required "longDesc" string
                |> required "startTime" (nullable int)
                |> required "endTime" int
                |> required "erc20Addr" string
                |> required "discussionLink" (nullable string)
                |> required "binding" bool
                |> required "encryptionPK" (nullable string)
                |> required "options" optionsDecoder
    in
    decodeValue decoder val


optionsDecoder : Decoder OptsOuter
optionsDecoder =
    resolve <|
        (decode handleOpts
            |> required "optionsVersion" int
            |> required "options" value
        )


handleOpts : Int -> Value -> Decoder OptsOuter
handleOpts v val =
    case v of
        1 ->
            decode (OptsSimple RangeVotingPlusMinus3)
                |> required "options" (list simpleOptionDecoder)

        2 ->
            decode (\_ -> OptsBinary)
                |> required "options" (null OptsBinary)

        _ ->
            fail "Unrecognised optionsVersion"


simpleOptionDecoder : Decoder SimpleOption
simpleOptionDecoder =
    decode SimpleOption
        |> required "optionTitle" string
        |> required "optionDesc" (nullable string)
