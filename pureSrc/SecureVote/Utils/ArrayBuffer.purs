module SecureVote.Utils.ArrayBuffer where

import Prelude

import Data.Array (cons, replicate)
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, dataView, toArray, toIntArray)
import Data.ArrayBuffer.Types (ArrayView, Uint8, Uint8Array)
import Data.Int (fromStringAs, hexadecimal, toNumber, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, joinWith, length, take, drop) as String
import Data.String.Yarn (reverse)
import Unsafe.Coerce (unsafeCoerce)


newtype UI8AShowable = UI8AShowable Uint8Array

justUI8A :: UI8AShowable -> Uint8Array
justUI8A (UI8AShowable a) = a

instance showUint8Array :: Show (UI8AShowable) where
  show a = unsafeCoerce a

instance eqUint8Array :: Eq UI8AShowable where
  eq (UI8AShowable a) (UI8AShowable b) = toArray a == toArray b


padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = String.fromCharArray <<< flip replicate c $ (len - String.length str)


padRight :: Char -> Int -> String -> String
padRight c len str = reverse $ padLeft c len $ reverse str 


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
