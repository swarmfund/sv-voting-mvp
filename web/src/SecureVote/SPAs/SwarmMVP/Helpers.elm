module SecureVote.SPAs.SwarmMVP.Helpers exposing (..)

import Char
import Dict
import Hex
import Keccak exposing (ethereum_keccak_256)
import Maybe.Extra exposing (combine, (?))
import Regex exposing (Regex, contains, regex)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(SetField))


swmAddrId =
    "ethAddress"


getSwmAddress : Model -> Maybe String
getSwmAddress model =
    Dict.get swmAddrId model.fields


setSwmAddress : String -> Msg
setSwmAddress =
    SetField swmAddrId
