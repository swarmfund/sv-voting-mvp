module SecureVote.SPAs.SwarmMVP.Web3Handler exposing (..)

import Json.Decode as Decode exposing (Value)
import SecureVote.Eth.Web3 exposing (ReadResponse)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))


decodeRead : ReadResponse -> Msg
decodeRead { success, errMsg, response, method } =
    case method of
        "getBallotOptions" ->
            if success then
                decodeBallotOpts response
            else
                MultiMsg [ LogErr errMsg, FromWeb3 <| GetBallotOpts ( Nothing, Just errMsg ) ]

        _ ->
            LogErr "Unknown method returned from web3"


decodeBallotOpts : Value -> Msg
decodeBallotOpts val =
    let
        llDecoder =
            Decode.list (Decode.list Decode.int)
    in
    case Decode.decodeValue llDecoder val of
        Ok opts ->
            FromWeb3 <| GetBallotOpts ( Just opts, Nothing )

        Err err ->
            MultiMsg [ LogErr err, FromWeb3 <| GetBallotOpts ( Nothing, Just err ) ]


readBallotOptsErr : String -> Msg
readBallotOptsErr errMsg =
    MultiMsg [ LogErr errMsg, FromWeb3 <| GetBallotOpts ( Nothing, Just errMsg ) ]
