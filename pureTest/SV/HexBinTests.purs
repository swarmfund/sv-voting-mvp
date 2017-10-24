module Test.SV.HexBinTests where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.TypedArray (asArray, asUint8Array)
import SecureVote.Utils.ArrayBuffer (UI8AShowable(..), fromHex, toHex)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


hexBinTests :: forall e. SpecType e
hexBinTests = do
  it "should convert too and from hex exactly" do
    quickCheck testHex
  it "should only accept hex with an even number of characters" do
    shouldBeNothing "0"
    shouldBeNothing "12345"
  it "should be good with some base cases" do
      "" `shouldMatch` []
      "00000000" `shouldMatch` [0,0,0,0]
      "10" `shouldMatch` [16]
      "1010" `shouldMatch` [16, 16]
      "0010" `shouldMatch` [0, 16]
      "ff" `shouldMatch` [255]
      "deadbeef" `shouldMatch` [222,173,190,239]
  where
    fromHex' a = UI8AShowable <$> fromHex a
    convArr = (Just <<< UI8AShowable <<< asUint8Array)
    shouldMatch hex arr = (fromHex' hex) `shouldEqual` (convArr arr)
    shouldBeNothing hex = (fromHex' hex) `shouldEqual` Nothing
        


testHex :: forall e. Array Number -> Boolean
testHex someInts = fromMaybe false $ do
    bs2 <- bsBack
    let arr1 = asArray bs1
    let arr2 = asArray bs2
    -- let _ = unsafePerformEff $ log $ unsafeCoerce bs1
    -- let _ = unsafePerformEff $ log $ unsafeCoerce hex
    pure $ (arr1 :: Array Int) == arr2
  where
    bs1 = asUint8Array $ map (flip mod 256 <<< round <<< abs <<< (*) 512.0) someInts
    hex = toHex bs1
    bsBack = fromHex hex
      
