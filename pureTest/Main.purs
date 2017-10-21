module Test.Main where

import Math
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Types (Uint8, Uint8Array)
import Data.Int (round)
import Data.Maybe (Maybe)
import Data.Maybe as M
import Data.TypedArray (asArray, asUint8Array)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex)
import Test.QuickCheck ((===), quickCheck, Result(..))

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, random :: RANDOM | e) Unit
main = do
  quickCheck testHex


testHex :: Array Number -> Boolean
testHex someInts = M.fromMaybe false $ 
  do
    bs2 <- bsBack
    let arr1 = asArray bs1
    let arr2 = asArray bs2
    pure $ (arr1 :: Array Int) == arr2
  where
    bs1 :: Uint8Array
    bs1 = asUint8Array $ map (flip mod 256 <<< round <<< abs) someInts
    hex :: String
    hex = toHex bs1
    bsBack :: Maybe (Uint8Array)
    bsBack = fromHex hex