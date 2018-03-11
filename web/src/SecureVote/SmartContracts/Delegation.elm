module SecureVote.SmartContracts.Delegation exposing (..)

import Dict
import Json.Decode exposing (Decoder, decodeValue, list, string)
import Json.Decode.Pipeline exposing (decode)
import Json.Encode as E
import List.Extra
import Maybe.Extra exposing ((?))
import Result.Extra
import SecureVote.Eth.Tasks exposing (decodeAndUnpackResp, readContract)
import SecureVote.Eth.Types exposing (zeroAddr)
import SecureVote.Eth.Utils exposing (ethAddrEq)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationResp, DelegatorsResp)
import SecureVote.Utils.DecodeP exposing (reqIndex, strInt)
import Task


decDelegationResp : Decoder DelegationResp
decDelegationResp =
    decode DelegationResp
        |> reqIndex 0 strInt
        |> reqIndex 1 strInt
        |> reqIndex 2 strInt
        |> reqIndex 3 string
        |> reqIndex 4 string
        |> reqIndex 5 string


decDelegatorsOfResp : Decoder DelegatorsResp
decDelegatorsOfResp =
    decode (\a b -> List.Extra.zip a b |> List.map (\( a, b ) -> { voterAddr = a, tokenAddr = b }))
        -- type of DelegatorsResp
        |> reqIndex 0 (list string)
        |> reqIndex 1 (list string)


type alias VotersForDlgtResp =
    { voterTokenPairs : DelegatorsResp
    }


type alias VotersByToken =
    Dict.Dict String (List String)


decVotersForDelegateResp =
    Json.Decode.map VotersForDlgtResp decDelegatorsOfResp


findDelegatorsOf : { scAddr : String, abi : String, dlgtAddr : String } -> Task.Task String VotersForDlgtResp
findDelegatorsOf { scAddr, abi, dlgtAddr } =
    readContract
        { addr = scAddr
        , abi = abi
        , args = E.list <| viewDelegatorsArgs dlgtAddr
        , method = "findPossibleDelegatorsOf"
        }
        |> decodeAndUnpackResp decVotersForDelegateResp
        |> Task.map (Debug.log "findDelegatorsOf returns:")


resolveDelegation : { scAddr : String, abi : String, voterAddr : String, tokenAddr : Maybe String } -> Task.Task String DelegationResp
resolveDelegation { scAddr, abi, voterAddr, tokenAddr } =
    readContract
        { addr = scAddr
        , abi = abi
        , args = E.list <| viewDelegationArgs ( voterAddr, tokenAddr ? zeroAddr )
        , method = "resolveDelegation"
        }
        |> decodeAndUnpackResp decDelegationResp


getVotersForDlgtTask : { scAddr : String, abi : String, dlgtAddr : String } -> Task.Task String VotersByToken
getVotersForDlgtTask ({ scAddr, abi, dlgtAddr } as findDsArgs) =
    findDelegatorsOf findDsArgs
        |> Task.andThen
            (\{ voterTokenPairs } ->
                Task.sequence <| List.map (\{ voterAddr, tokenAddr } -> resolveDelegation { scAddr = scAddr, abi = abi, voterAddr = voterAddr, tokenAddr = Just tokenAddr }) voterTokenPairs
            )
        |> Task.map
            (List.filter (\{ delegatee } -> ethAddrEq delegatee dlgtAddr)
                >> List.foldl (\{ delegator, tokenAddr } -> Dict.update tokenAddr (Maybe.withDefault [] >> (::) delegator >> Just)) Dict.empty
            )


setDelegationArgs : { tokenAddr : Maybe String, dlgtAddr : String } -> List E.Value
setDelegationArgs { tokenAddr, dlgtAddr } =
    case tokenAddr of
        Just tAddr ->
            [ E.string tAddr
            , E.string dlgtAddr
            ]

        Nothing ->
            [ E.string dlgtAddr ]


viewDelegationArgs : ( String, String ) -> List E.Value
viewDelegationArgs ( voterAddr, tokenAddr ) =
    [ E.string voterAddr
    , E.string tokenAddr
    ]


viewDelegatorsArgs : String -> List E.Value
viewDelegatorsArgs dlgtAddr =
    [ E.string <| dlgtAddr ]
