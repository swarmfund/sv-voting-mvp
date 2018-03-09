module SecureVote.SmartContracts.BallotBox exposing (..)

import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import SecureVote.Utils.DecodeP exposing (reqIndex, strInt)


type alias BallotMapEntry =
    { ballotData : String
    , sender : String
    , blockN : Int
    }


ballotMapDecoder : D.Decoder BallotMapEntry
ballotMapDecoder =
    P.decode BallotMapEntry
        |> reqIndex 0 D.string
        |> reqIndex 1 D.string
        |> reqIndex 2 strInt
