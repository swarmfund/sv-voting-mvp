module SecureVote.SPAs.SwarmMVP.Helpers exposing (..)

import Dict
import ParseInt exposing (parseIntRadix, toRadix)
import Result.Extra exposing (isOk)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(SetField))
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits, intsToRangeBallot3Bits)


swmAddrId : String
swmAddrId =
    "ethAddress"


getSwmAddress : Model -> Maybe String
getSwmAddress model =
    Dict.get swmAddrId model.fields


setSwmAddress : String -> Msg
setSwmAddress =
    SetField swmAddrId


delegateAddrId : String
delegateAddrId =
    "delegateAddress"


getDelegateAddress : Model -> Maybe String
getDelegateAddress model =
    Dict.get delegateAddrId model.fields


setDelegateAddress : String -> Msg
setDelegateAddress =
    SetField delegateAddrId


ethNodeTemp : String
ethNodeTemp =
    "ethNodeUrl"


getEthNodeTemp : Model -> Maybe String
getEthNodeTemp model =
    Dict.get ethNodeTemp model.fields


setEthNodeTemp : String -> Msg
setEthNodeTemp =
    SetField ethNodeTemp


ballotRangeAbs : Int
ballotRangeAbs =
    3



-- This describes the number of options a user can select, from -1*ballotRangeAbs to ballotRangeAbs


ballotRangeSize : Int
ballotRangeSize =
    ballotRangeAbs * 2 + 1


ballotMax : Int
ballotMax =
    ballotRangeAbs * 2


ballotDisplayMax : Int
ballotDisplayMax =
    0 + ballotRangeAbs


ballotDisplayMin : Int
ballotDisplayMin =
    0 - ballotRangeAbs


ballotValToBytes : Int -> Result String RangeBallot3Bits
ballotValToBytes value =
    let
        -- this will change -3 to 0, 0 to +3, and +3 to 6, for example (with ballotRangeAbs = 3)
        absValue =
            value + ballotRangeAbs

        binaryValueString =
            toRadix 2 absValue
                |> Result.map (String.padLeft 3 '0')

        -- this will ensure we only have 1s and 0s in our string due to parseIntRadix
        reencodedOkay =
            binaryValueString
                |> Result.andThen (parseIntRadix 2)
                |> Result.mapError toString
                |> Result.andThen
                    (\val ->
                        if val < 0 || val > ballotMax then
                            Err "Ballot value out of range"
                        else
                            Ok True
                    )

        procString =
            String.split "" >> List.map String.toInt >> Result.Extra.combine >> Result.andThen intsToRangeBallot3Bits
    in
    case reencodedOkay of
        Ok _ ->
            case binaryValueString of
                Ok str ->
                    procString str

                Err err ->
                    Err <| "Error parsing ballot: " ++ toString err

        Err msg ->
            Err <| "Ballot failed reencoding check: " ++ msg


codepointToBinary : Int -> Result ParseInt.Error String
codepointToBinary =
    Result.map (String.padLeft 8 '0') << toRadix 2
