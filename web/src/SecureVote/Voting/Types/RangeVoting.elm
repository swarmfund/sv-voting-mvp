module SecureVote.Voting.Types.RangeVoting exposing (..)

import SecureVote.Types.VBit exposing (Len3, SafeList, VBit, intsToVBits, vblFromList, vblLen3)


type alias RangeBallot3Bits =
    -- This is a list that is GUARANTEED to be of length 3
    SafeList VBit Len3


intsToRangeBallot3Bits : List Int -> Result String RangeBallot3Bits
intsToRangeBallot3Bits ints =
    let
        mVBits =
            intsToVBits ints
    in
    mVBits
        |> Maybe.andThen (vblFromList vblLen3)
        |> Result.fromMaybe "Conversion to RangeBallot3Bits failed converting to bits"
