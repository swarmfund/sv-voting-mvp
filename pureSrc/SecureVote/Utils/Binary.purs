module SecureVote.Utils.Binary where


import SV.Prelude

import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ByteString (ByteString, Encoding(..), Octet, fromString, pack, toString, unpack)
import Data.ByteString as BS
import Data.Char (toCharCode)
import Data.Int (fromStringAs, hexadecimal, toNumber, toStringAs)
import Data.String as S
import Network.Ethereum.Web3 (BytesN, D2, fromByteString, mkHexString, unHex)
import Network.Ethereum.Web3.Types.Types (HexString)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Type.Quotient (mkQuotient, runQuotient)


mkBytes2 :: Tuple Octet Octet -> BytesN D2
mkBytes2 (Tuple o1 o2) = unsafePartialBecause "We're guaranteed to have exactly 2 bytes"
                $ fromJust
                $ fromByteString
                $ pack [o1, o2]


char2Octs :: Char -> Char -> Tuple Octet Octet
char2Octs a b = Tuple (go a) (go b)
  where go = mkQuotient <<< toCharCode


char2Bytes2 :: Char -> Char -> BytesN D2
char2Bytes2 a b = mkBytes2 $ char2Octs a b


strToBS :: String -> ByteString
strToBS s = unsafePartialBecause "UTF16 strings can always be turned into ByteStrings"
            $ fromJust
            $ fromString s UTF16LE

bsToStr :: ByteString -> Maybe String
bsToStr bs = Just $ toString bs UTF16LE

-- TODO: bsTake and bsDrop are O(n), any way to improve?
bsTake :: Int -> ByteString -> Maybe ByteString
bsTake i bs = if BS.length ans == i then Just ans else Nothing
  where
    ans = pack $ A.take i $ unpack bs

bsDrop :: Int -> ByteString -> Maybe ByteString
bsDrop i bs = if BS.length bs < i then Nothing else Just ans
  where
    ans = pack $ A.drop i $ unpack bs

toHex :: ByteString -> String
toHex bs = BS.toString bs BS.Hex

fromHex :: String -> Maybe ByteString
fromHex hex = BS.fromString hex BS.Hex

intToBS :: Int -> ByteString
intToBS i = unsafePartialBecause "we just generated the hex" $ fromJust $ BS.fromString iHex_ BS.Hex
  where
    iHex = toStringAs hexadecimal i
    iHex_ = if S.length iHex `mod` 2 /= 0 then "0" <> iHex else iHex

bsToInt :: ByteString -> Int
bsToInt bs = unsafePartialBecause "all bytestrings correspond to an int" $ fromJust $ fromStringAs hexadecimal (toHex bs)

bsToUi8a :: ByteString -> Uint8Array
bsToUi8a bs = bs # unpack # map runQuotient # map toNumber # fromArray # whole # asUint8Array

ui8aToBs :: Uint8Array -> ByteString
ui8aToBs bs = bs # toIntArray # map mkQuotient # pack

ui8aToBase64 :: Uint8Array -> String
ui8aToBase64 bs = bs # ui8aToBs # bsToBase64

ui8aFromBase64 :: String -> Maybe Uint8Array
ui8aFromBase64 s = s # bsFromBase64 <#> bsToUi8a

bsToBase64 :: ByteString -> String
bsToBase64 = flip BS.toString Base64

bsFromBase64 :: String -> Maybe ByteString
bsFromBase64 = flip BS.fromString Base64

eqUI8A :: Uint8Array -> Uint8Array -> Boolean
eqUI8A u1 u2 = toIntArray u1 == toIntArray u2

hexToUi8a :: String -> Maybe Uint8Array
hexToUi8a str = bsToUi8a <$> fromHex str

lenUi8a :: Uint8Array -> Int
lenUi8a bs = BS.length $ ui8aToBs bs

hexStrToBs :: HexString -> ByteString
hexStrToBs = unsafePartial fromJust <<< fromHex <<< unHex

bsToHexStr :: ByteString -> HexString
bsToHexStr = unsafePartial fromJust <<< mkHexString <<< toHex
