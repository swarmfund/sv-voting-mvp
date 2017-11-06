module SecureVote.Utils.Decimal where

import Data.Decimal (Decimal)

foreign import toFixed :: Int -> Decimal -> String