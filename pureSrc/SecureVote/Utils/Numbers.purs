module SecureVote.Utils.Numbers where


import Prelude

import Data.Int (binary, decimal, toStringAs)
import SecureVote.Utils.ArrayBuffer (padLeft)


intToStr :: Int -> String
intToStr = toStringAs decimal


intByteToBitStr :: Int -> String
intByteToBitStr = padLeft '0' 8 <<< toStringAs binary