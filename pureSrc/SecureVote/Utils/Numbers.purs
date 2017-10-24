module SecureVote.Utils.Numbers where


import Prelude
import Data.Int (decimal, toStringAs)


intToStr :: Int -> String
intToStr = toStringAs decimal
