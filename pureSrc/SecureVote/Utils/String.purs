module SecureVote.Utils.String where

import Prelude

import Data.String as String
import Data.String.Yarn (replicate, reverse)

padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = flip replicate c $ (len - String.length str)


padRight :: Char -> Int -> String -> String
padRight c len str = reverse $ padLeft c len $ reverse str 
