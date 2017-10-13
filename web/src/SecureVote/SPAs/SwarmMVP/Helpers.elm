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


hexRegex : Regex
hexRegex =
    regex "^[0-9a-fA-F]*$"


addrRegex : Regex
addrRegex =
    regex "^0x[0-9a-fA-F]{40}$"


upperCaseHexRegex : Regex
upperCaseHexRegex =
    regex "[A-F]"


isValidEthAddress : String -> Bool
isValidEthAddress str =
    if contains upperCaseHexRegex str then
        toChecksumAddress str ? "" == str && contains addrRegex str
    else
        contains addrRegex str


toHex : List Int -> Maybe String
toHex ints =
    let
        intToHex i =
            let
                midHex =
                    Hex.toString i

                finHex =
                    if String.length midHex == 1 then
                        "0" ++ midHex
                    else
                        midHex
            in
                if i < 0 || i > 255 then
                    Nothing
                else
                    Just finHex
    in
        (combine <| List.map intToHex ints) |> Maybe.map String.concat


toChecksumAddress : String -> Maybe String
toChecksumAddress addr =
    let
        noPrefix =
            String.dropLeft 2 <| String.toLower addr

        hashM =
            toHex <| ethereum_keccak_256 <| List.map Char.toCode <| String.toList noPrefix

        -- checksummedChar
        csdChar aChar i =
            if i >= 8 then
                Char.toUpper aChar
            else
                aChar

        checksumCaseR hChar aChar =
            Maybe.map (csdChar aChar) (Result.toMaybe <| Hex.fromString <| String.fromChar hChar)

        convertedCase =
            hashM |> Maybe.andThen (\hash -> combine <| List.map2 (checksumCaseR) (String.toList hash) (String.toList noPrefix))
    in
        convertedCase |> Maybe.map ((++) "0x" << String.fromList)
