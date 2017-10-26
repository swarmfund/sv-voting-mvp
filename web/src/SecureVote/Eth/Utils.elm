module SecureVote.Eth.Utils exposing (..)

import Char
import Decimal exposing (Decimal)
import Hex
import Keccak exposing (ethereum_keccak_256)
import Maybe.Extra exposing ((?), combine)
import ParseInt
import Regex exposing (Regex, contains, regex)
import SecureVote.Eth.Models exposing (CandidateEthTx, MinEthTx)


setCandTxFrom : String -> CandidateEthTx -> CandidateEthTx
setCandTxFrom from tx =
    { tx | from = Just from }


processCandidateTx : CandidateEthTx -> Maybe MinEthTx
processCandidateTx candTx =
    let
        from =
            candTx.from

        to =
            candTx.to

        value =
            candTx.value

        data =
            candTx.data

        gas =
            candTx.gas
    in
    -- Warning : because most of these are strings we must be very careful with ordering
    Maybe.map5 MinEthTx from to (Just value) data (Just gas)


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


fromHex : String -> Maybe (List Int)
fromHex str =
    let
        byte =
            Result.toMaybe <| ParseInt.parseIntHex <| String.slice 0 2 str

        remStr =
            String.dropLeft 2 str
    in
    if str == "" then
        Just []
    else if String.length str % 2 == 0 then
        Maybe.map2 (\b bs -> b :: bs) byte (fromHex remStr)
    else
        Nothing


toHexEth : List Int -> Maybe String
toHexEth ints =
    Maybe.map ((++) "0x") (toHex ints)


fromHexEth : String -> Maybe (List Int)
fromHexEth str =
    if String.slice 0 2 str == "0x" then
        fromHex <| String.dropLeft 2 str
    else
        Nothing


intToHexEth : Int -> Maybe String
intToHexEth input =
    let
        outHex_ =
            Hex.toString input

        outHex =
            if String.length outHex_ % 2 == 1 then
                "0" ++ outHex_
            else
                outHex_
    in
    if input < 0 then
        Nothing
    else
        Just <| "0x" ++ outHex


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
            hashM |> Maybe.andThen (\hash -> combine <| List.map2 checksumCaseR (String.toList hash) (String.toList noPrefix))
    in
    convertedCase |> Maybe.map ((++) "0x" << String.fromList)


dec10ToThe18 : Decimal
dec10ToThe18 =
    Decimal.fromIntWithExponent 1 18


dec10ToTheNeg18 : Decimal
dec10ToTheNeg18 =
    Decimal.fromIntWithExponent 1 -18


dec0 : Decimal
dec0 =
    Decimal.zero


decimalTo18dps : Decimal -> Decimal
decimalTo18dps toDiv =
    Decimal.mul toDiv dec10ToTheNeg18


decimalFrom18dps : Decimal -> Decimal
decimalFrom18dps toMul =
    Decimal.mul toMul dec10ToThe18


stripTrailingZeros : String -> String
stripTrailingZeros str =
    let
        isZero =
            String.endsWith "0" str
    in
    if isZero then
        stripTrailingZeros <| String.dropRight 1 str
    else
        str


stripTrailingDecimalPoint : String -> String
stripTrailingDecimalPoint str =
    let
        isDec =
            String.endsWith "." str
    in
    if isDec then
        stripTrailingDecimalPoint <| String.dropRight 1 str
    else
        str


addCommasToBalance : String -> String
addCommasToBalance str =
    let
        postSplit =
            String.split "." str

        ( pre, post ) =
            case postSplit of
                pre_ :: post_ ->
                    ( pre_, post_ )

                [] ->
                    ( str, [] )

        last3 =
            String.right 3 pre

        firstLot =
            String.dropRight 3 pre
    in
    if String.length str <= 3 then
        str
    else
        String.join "." <| (addCommasToBalance firstLot ++ "," ++ last3) :: post


formatBalance : String -> String
formatBalance =
    addCommasToBalance << stripTrailingDecimalPoint << stripTrailingZeros


rawTokenBalance18DpsToBalance : Decimal -> String
rawTokenBalance18DpsToBalance =
    formatBalance << Decimal.toString << decimalTo18dps
