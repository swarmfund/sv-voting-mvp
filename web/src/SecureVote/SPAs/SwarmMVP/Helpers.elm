module SecureVote.SPAs.SwarmMVP.Helpers exposing (..)

import Dict
import ParseInt exposing (parseIntRadix, toRadix)
import Result.Extra exposing (isOk)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(SetField))


swmAddrId : String
swmAddrId =
    "ethAddress"


getSwmAddress : Model -> Maybe String
getSwmAddress model =
    Dict.get swmAddrId model.fields


setSwmAddress : String -> Msg
setSwmAddress =
    SetField swmAddrId


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


ballotValToBytes : Int -> Result String (List Int)
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
            String.split "" >> List.map String.toInt >> Result.Extra.combine
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
