module SecureVote.Democs.SwarmMVP.Ballot where


import Control.Monad.Eff
import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM, random, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (toUint8Array)
import Data.Array (range, (:))
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.ByteString (Encoding(..), fromString, toString)
import Data.Int (binary, fromStringAs, toNumber, toStringAs)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing, maybe)
import Data.String (Pattern(..), Replacement(..), drop, joinWith, replace, split, take)
import Data.String.Yarn (reverse)
import Data.Traversable (foldl, sequence)
import Data.TypedArray (asArray)
import Node.Buffer (Octet)
import Node.Encoding (Encoding(..))
import SecureVote.Utils.ArrayBuffer (fromHex, padLeft, padRight, toHex)
import Unsafe.Coerce (unsafeCoerce)


delegateAddr = "0x1111111111111111111111111111111111111111"


arrayToUint8A = asUint8Array <<< whole <<< fromArray 


makeBallot :: forall e. String -> Eff (random :: RANDOM | e) Uint8Array
makeBallot delegate = do
    ballots <- randInts 4
    let ballotsBits = padRight '0' 16 $ joinWith "" $ map ballotToBitStr ballots
    let ballotsBytes = bitStrToBytes ballotsBits
    let finalBytes = arrayToUint8A $ ballotsBytes <> (asArray procDelegateA)
    pure $ finalBytes
  where
    procDelegateM = fromHex $ take (14 * 2) $ replace (Pattern "0x") (Replacement "") delegate
    procDelegateA = fromMaybe (arrayToUint8A []) procDelegateM


randInts :: forall e. Int -> Eff (random :: RANDOM | e) (Array Int)
randInts 0 = pure []
randInts n = do 
    r <- randomInt 0 6
    rs <- randInts (n-1)
    pure $ r : rs


ballotToBitStr :: Int -> String
ballotToBitStr ballot = padLeft '0' 3 $ toStringAs binary ballot


bitStrToBytes :: String -> Array Number
bitStrToBytes "" = []
bitStrToBytes str = case byteM of 
        Nothing -> bytes
        Just byte -> (toNumber byte) : bytes
    where
        byteM = fromStringAs binary $ take 8 str 
        bytes = bitStrToBytes $ drop 8 str 



-- ballotBitsToBytes :: Maybe (Array Int) -> Maybe Uint8Array
-- ballotBitsToBytes bits =  bitsHexM >>= (\bitsHex-> fromHex $ bitsHex <> procDelegate)
--     where
--         justBitsAsStr = joinWith "" $ map ((toStringAs binary) <<< (flip mod 2)) bits
--         paddedBits = padRight '0' 16 justBitsAsStr
--         (bitsHexM :: Maybe String) = map (\bs -> toString bs Hex) $ fromString paddedBits Binary
--         -- take first 14 bytes of address
--         procDelegate = take (14 * 2) $ replace (Pattern "0x") (Replacement "") delegateAddr



