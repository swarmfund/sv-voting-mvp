module SV.Utils.Votes where

import SV.Prelude

import Data.Int (binary, ceil, toNumber, toStringAs)
import Math (log)
import SecureVote.Utils.String (padLeft)


voteNToBitStr :: {rangeMax :: Int} -> Int -> String
voteNToBitStr {rangeMax} ballot = padLeft '0' (nBitsForRangeVote rangeMax) $ toStringAs binary ballot


-- need to add 1 here because a rangeMax of 2^n has (2^n + 1) options and requires n+1 bits
nBitsForRangeVote :: Int -> Int
nBitsForRangeVote rangeMax = ceil $ log (toNumber rangeMax + 1.0) / log 2.0
