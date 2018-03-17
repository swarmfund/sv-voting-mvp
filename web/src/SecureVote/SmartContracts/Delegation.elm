module SecureVote.SmartContracts.Delegation
    exposing
        ( VotersByToken
        , decDelegationResp
        , decDelegatorsOfResp
        , findDelegatorsOf
        , getFullDelegatedBalance
        , getVotersForDlgtRecursive
        , getVotersForDlgtTask
        , resolveDelegation
        , setDelegationArgs
        , viewDelegationArgs
        , viewDelegatorsArgs
        )

import Decimal
import Dict
import Json.Decode exposing (Decoder, decodeValue, list, string)
import Json.Decode.Pipeline exposing (decode)
import Json.Encode as E
import List.Extra
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Tasks exposing (decodeAndUnpackResp, readContract)
import SecureVote.Eth.Types exposing (zeroAddr)
import SecureVote.Eth.Utils exposing (ethAddrEq)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationResp, DelegatorsResp)
import SecureVote.SmartContracts.Erc20 exposing (balanceOf)
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


decVotersForDelegateResp : Decoder VotersForDlgtResp
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
        |> Task.map (Dict.map (\k v -> List.Extra.unique v))
        |> Task.map (Debug.log <| "getVotersForDlgtTask (" ++ dlgtAddr ++ ") returning")


getVotersForDlgtInToken : { scAddr : String, abi : String, dlgtAddr : String, tknAddr : String } -> Task.Task String (List String)
getVotersForDlgtInToken { scAddr, abi, dlgtAddr, tknAddr } =
    getVotersForDlgtTask { scAddr = scAddr, abi = abi, dlgtAddr = dlgtAddr }
        |> Task.andThen
            (\vbt ->
                let
                    easyVoters =
                        Dict.get tknAddr vbt ? []

                    possVoters =
                        Dict.get zeroAddr vbt ? []
                in
                Task.sequence (List.map (\v -> resolveDelegation { scAddr = scAddr, abi = abi, voterAddr = v, tokenAddr = Just tknAddr }) possVoters)
                    |> Task.map
                        (List.filter (\{ delegatee } -> ethAddrEq delegatee dlgtAddr)
                            >> List.map (\{ delegator } -> delegator)
                            >> (++) easyVoters
                            >> List.Extra.unique
                        )
            )
        |> Task.map (Debug.log ("getVotersForDlgtInToken (tkn: " ++ tknAddr ++ ", dlgt: " ++ dlgtAddr ++ ") got voters:"))


type alias GetVotersForDlgtRecInput =
    { scAddr : String, abi : String, dlgtAddr : String, tknAddr : String, carryVtrs : List String }


getVotersForDlgtRecursive : GetVotersForDlgtRecInput -> Task.Task String (List String)
getVotersForDlgtRecursive ({ scAddr, abi, dlgtAddr, tknAddr, carryVtrs } as findDsArgs) =
    let
        tknAddrLower =
            String.toLower tknAddr
    in
    getVotersForDlgtInToken { scAddr = scAddr, abi = abi, dlgtAddr = dlgtAddr, tknAddr = tknAddrLower }
        |> Task.andThen
            (Debug.log ("getVotersForDlgtRecursive (tkn: " ++ tknAddrLower ++ ", dlgt: " ++ dlgtAddr ++ ") got voters:")
                {- [delgators] -> then create [Task [delegators2]]) for each delegator -}
                >> (\vtrs ->
                        let
                            carry =
                                List.Extra.unique <| vtrs ++ carryVtrs

                            {- ensure we filter out voters already processed (in carry) -}
                            fliteredVtrs =
                                Debug.log "getVotersForDlgtRecursive filteredVtrs: " <|
                                    List.filter (\v -> not <| List.member v carryVtrs) vtrs
                        in
                        [ Task.succeed vtrs ]
                            ++ List.map (\v -> getVotersForDlgtRecursive { findDsArgs | carryVtrs = carry, dlgtAddr = v }) fliteredVtrs
                   )
                >> Task.sequence
            )
        -- at this point we have a Task [delegators], so we're done
        |> Task.map (List.concat >> List.Extra.unique)
        |> Task.map (Debug.log ("getVotersForDlgtRecursive for " ++ dlgtAddr ++ " got: "))


getFullDelegatedBalance : { erc20Addr : String, delegationAddr : String, delegationAbi : String, dlgtAddr : String, atBlock : String } -> Task.Task String Decimal.Decimal
getFullDelegatedBalance { erc20Addr, delegationAddr, delegationAbi, dlgtAddr, atBlock } =
    getVotersForDlgtRecursive { scAddr = delegationAddr, abi = delegationAbi, dlgtAddr = dlgtAddr, tknAddr = erc20Addr, carryVtrs = [] }
        -- returns list of voters
        |> Task.map ((::) dlgtAddr >> List.Extra.unique)
        -- add dlgt address to this list
        |> Task.andThen (List.map (\v -> balanceOf { scAddr = erc20Addr, addr = v, atBlock = atBlock }) >> Task.sequence)
        |> Task.map (List.foldl (\a b -> Decimal.add a b) Decimal.zero)
        |> Task.map (Debug.log ("getFullDlgtdBalance for " ++ dlgtAddr ++ " got: "))


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
