module SV.Utils.Binary where

import SV.Prelude

import Data.ByteString (Octet)
import Data.Int (binary, toStringAs)
import SecureVote.Utils.String (padLeft)
import Type.Quotient (runQuotient)


octetToBitStr :: Octet -> String
octetToBitStr = padLeft '0' 8 <<< toStringAs binary <<< runQuotient
