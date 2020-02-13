module SecureVote.Voting.Types.RangeVoting exposing (..)

import SecureVote.Types.VBit exposing (Len3, SafeList, VBit, intsToVBits, vblFromList, vblLen3, vblCons, vblNull, VBit(..))


type alias RangeBallot3Bits =
    -- This is a list that is GUARANTEED to be of length 3
    SafeList VBit Len3


-- This is 011 which is 3 in binary; since in range voting a vote of -3 maps to a binary representation of 0,
-- a vote of 0 (neutral) maps to 3
uninitializedWeightingForRangeVotingPlusMinusThree =
    vblCons ZeroBit (vblCons OneBit (vblCons OneBit vblNull))


intsToRangeBallot3Bits : List Int -> Result String RangeBallot3Bits
intsToRangeBallot3Bits ints =
    let
        mVBits =
            intsToVBits ints
    in
    mVBits
        |> Maybe.andThen (vblFromList vblLen3)
        |> Result.fromMaybe "Conversion to RangeBallot3Bits failed converting to bits"
