module SecureVote.SPAs.SwarmMVP.Web3Handler exposing (..)

import Json.Decode as Decode exposing (Value)
import SecureVote.Eth.Web3 exposing (ReadResponse)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..))


decodeRead : ReadResponse -> Msg
decodeRead { success, errMsg, response, method } =
    if success then
        case method of
            "getBallotOptions" ->
                decodeBallotOpts response

            _ ->
                NoOp
    else
        LogErr errMsg


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
