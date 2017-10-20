module SecureVote.Types.VBit exposing (..)

import Maybe.Extra
import SecureVote.List.Utils exposing (padRight)
import SecureVote.Types.TypeNat exposing (OnePlus, Zero)


-- SafeList heavily inspired by: https://github.com/JoeyEremondi/safelist
-- Todo: Factor out VBit stuff and make general


type VBit
    = ZeroBit
    | OneBit


intToVBit : Int -> Maybe VBit
intToVBit i =
    case i of
        1 ->
            Just OneBit

        0 ->
            Just ZeroBit

        _ ->
            Nothing


type SafeList a n
    = SafeList (List a)


type alias Len3 =
    OnePlus (OnePlus (OnePlus Zero))


vblNull : SafeList VBit Zero
vblNull =
    SafeList []


vblLen3 : SafeList VBit Len3
vblLen3 =
    vblCons ZeroBit (vblCons ZeroBit (vblCons ZeroBit vblNull))


vblCons : VBit -> SafeList VBit n -> SafeList VBit (OnePlus n)
vblCons vb (SafeList vbs) =
    SafeList (vb :: vbs)


vblUncons : SafeList VBit (OnePlus n) -> ( VBit, SafeList VBit n )
vblUncons vbs =
    case vbs of
        SafeList (vb :: vbs_) ->
            ( vb, SafeList vbs_ )

        _ ->
            Debug.crash "Type error: VBitList should never be an empty list since OnePlus is in the type sig"


vblFromList : SafeList VBit n -> List VBit -> Maybe (SafeList VBit n)
vblFromList (SafeList listOfLen) bitList =
    if List.length bitList == List.length listOfLen then
        Just <| SafeList bitList
    else
        Nothing


vblToList : SafeList VBit n -> List VBit
vblToList (SafeList vbits) =
    vbits


intsToVBits : List Int -> Maybe (List VBit)
intsToVBits ints =
    Maybe.Extra.combine <| List.map intToVBit ints


vBitsToInt : List VBit -> Int
vBitsToInt vbits =
    let
        len =
            List.length vbits
    in
    case vbits of
        vbit :: vbits_ ->
            vBitsToInt vbits_
                + (if vbit == OneBit then
                    2 ^ (len - 1)
                   else
                    0
                  )

        [] ->
            0


vBitsToBytes : List VBit -> List Int
vBitsToBytes vbits =
    let
        byte1 =
            vBitsToInt <| padRight 8 ZeroBit <| List.take 8 vbits

        remBits =
            List.drop 8 vbits
    in
    if List.length vbits == 0 then
        []
    else
        byte1 :: vBitsToBytes remBits
