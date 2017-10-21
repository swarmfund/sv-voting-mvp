module SecureVote.Utils.ArrayBuffer where

import Prelude

import Data.Array (cons, replicate)
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (toIntArray, dataView, asUint8Array)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int (fromStringAs, hexadecimal, toNumber, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, joinWith, length, take, drop) as String


padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = String.fromCharArray (replicate (len - String.length str) c)


toHex :: Uint8Array -> String
toHex bs = (String.joinWith "") $ map (pad <<< intConv) $ toIntArray bs
    where range = DV.byteLength $ dataView bs
          intConv = toStringAs hexadecimal
          pad = padLeft '0' 2


fromHex :: String -> Maybe Uint8Array
fromHex str = map (asUint8Array <<< DV.whole <<< fromArray <<< map toNumber) mInts
  where mInts = hexToIntList str


hexToIntList :: String -> Maybe (Array Int)
hexToIntList "" = Just []
hexToIntList str = 
    if ((String.length str) `mod` 2 /= 0) then
      Nothing
    else
      do
        b <- fromStringAs hexadecimal $ String.take 2 str
        bs <- hexToIntList $ String.drop 2 str 
        pure $ b `cons` bs
