module SecureVote.SPAs.SwarmMVP.SubHandlers exposing (..)

import Json.Decode as D exposing (Value, decodeValue, int, string)
import Json.Decode.Pipeline as P
import Json.Encode as E
import SecureVote.Eth.Msg exposing (..)
import SecureVote.Eth.Types exposing (..)
import SecureVote.LocalStorage exposing (..)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (..)
import SecureVote.SmartContracts.BallotBox exposing (ballotMapDecoder)
import SecureVote.Utils.DecodeP exposing (reqIndex, strInt)


lsGetHandler : Result String LsEntry -> Msg
lsGetHandler r =
    case r of
        Ok { key, value } ->
            case key of
                _ ->
                    LS <| LsGeneral ( key, value )

        Err str ->
            LogErr <| "LocalStorage deserialisation error: " ++ str


lsFailHandler : Result String LsError -> Msg
lsFailHandler r =
    case r of
        Ok { key, errMsg } ->
            LS <| LsNotFound key

        Err str ->
            LogErr <| "LocalStorage deserialisation error: " ++ str


cReadHandler : Model -> ReadResponse -> Msg
cReadHandler model r =
    let
        rAndThenPack rSecond rFirst =
            Result.map2 (\fst snd -> ( fst, snd )) rFirst rSecond

        bMapCarryDecoder =
            P.decode (\a b -> ( a, b ))
                |> reqIndex 0 string
                |> reqIndex 1 int

        resultNext =
            if r.success then
                case r.method of
                    "voterToBallotID" ->
                        decodeValue string r.carry.payload
                            |> rAndThenPack (decodeValue (D.index 0 strInt) r.response)
                            |> Result.map
                                (\( bHash, i ) ->
                                    if i == 0 then
                                        let
                                            newCarry =
                                                { hops = r.carry.hops, payload = E.list [ r.carry.payload, E.int i ] }
                                        in
                                        Web3 <| ReadContract { addr = r.addr, method = "ballotMap", abi = model.ballotBoxABI, args = [ r.response ], carry = newCarry }
                                    else
                                        MarkBallotVoted True { voterM = Nothing, bHash = bHash }
                                )

                    "ballotMap" ->
                        decodeValue bMapCarryDecoder r.carry.payload
                            |> rAndThenPack (decodeValue ballotMapDecoder r.response)
                            |> Result.map
                                (\( ( bHash, i ), bltInfo ) ->
                                    if bltInfo.blockN == 0 then
                                        MultiMsg [ MarkBallotVoted False { voterM = Just bltInfo.sender, bHash = bHash }, MarkBallotVoted False { voterM = Nothing, bHash = bHash } ]
                                    else
                                        MultiMsg [ MarkBallotVoted True { voterM = Just bltInfo.sender, bHash = bHash }, MarkBallotVoted False { voterM = Nothing, bHash = bHash } ]
                                )

                    _ ->
                        Err "Web3 contract read - unknown method"
            else
                Err <| "Web3 call failed: " ++ r.errMsg
    in
    case resultNext of
        Ok m ->
            m

        Err e ->
            LogErr e
