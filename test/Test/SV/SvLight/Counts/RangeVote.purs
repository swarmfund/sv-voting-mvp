module Test.SV.SvLight.Counts.RangeVote where

import SV.Prelude

import Control.Apply (lift2)
import Control.Monad.Aff.Console as AffC
import Data.Array (foldl)
import Data.Array as Arr
import Data.ByteString as BS
import Data.Int (round)
import Data.Lens ((.~))
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Record.ShowRecord (showRecord)
import Network.Ethereum.Web3 (embed, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec)
import SV.Light.Counts (RangeOffset(..), countRange)
import SV.Light.Delegation (findDelegatorsRecursive)
import SV.Light.SmartContract (mkSC)
import SV.Light.Types.Ballot (BallotSpec(..), OptsOuter(..))
import SV.Light.Types.RunBallot (SmartContract)
import SV.Utils.Votes (voteNToBitStr)
import SecureVote.Democs.SwarmMVP.Ballot (bitStrToBytes)
import SecureVote.Utils.Binary (bsToHexStr)
import SecureVote.Utils.Numbers (intByteToBitStr)
import SecureVote.Utils.String (padRight)
import SecureVote.Web3.Web3 (EthNetwork(..), setNetwork)
import Test.SV.Types (SpecType)
import Test.Spec (it, pending)
import Test.Spec.Assertions (shouldEqual)
import Type.Quotient (mkQuotient)


upmhx = unsafePartial fromJust <<< mkHexString
upma = unsafePartial fromJust <<< (mkAddress <=< mkHexString)


zAddr = upma "0x0000000000000000000000000000000000000000"


mkSimOpt optionTitle = wrap {optionTitle, optionDesc: Nothing}
mkBallot rangeMax vs bal =
                    vs <#> voteNToBitStr {rangeMax}
                    # Arr.foldr (<>) ""
                    # padRight '0' (8 * 32)
                    # bitStrToBytes >>> map round
                    # map mkQuotient >>> BS.pack
                    # bsToHexStr
                    # \b -> {origVoter: zAddr, bal, ballot: {i: 0, voterAddr: zAddr, voterPk: upmhx "00", ballot: b}}


rangeVoteTests :: SpecType _
rangeVoteTests = do
    it "should count simple rangevotes" simpleRV
    it "should count slight complex rangevotes" complex1RV


simpleRV :: Aff _ Unit
simpleRV = do
        let res = countRange
                (RangeAbsolute {rangeMax: 2})
                [mkSimOpt "opt1", mkSimOpt "opt2"]
                [ mkBallot 2 [0,2] (embed 7)
                , mkBallot 2 [1,1] (embed 3)
                ]

        let mRes = Arr.foldr (\{name, count} -> Map.insert name count) Map.empty res

        Map.lookup "opt1" mRes `shouldEqual` Just (embed 3)
        Map.lookup "opt2" mRes `shouldEqual` Just (embed 17)
        Map.size mRes `shouldEqual` 2
        pure unit


complex1RV :: Aff _ Unit
complex1RV = do
        let res = countRange
                (RangePlusMinus {magnitude: 3})
                [mkSimOpt "1", mkSimOpt "2", mkSimOpt "3"]
                [ mkBallot 6 [0,3,6] (embed 7)
                , mkBallot 6 [1,2,4] (embed 3)
                , mkBallot 6 [5,3,1] (embed 2)
                ]

        let mRes = Arr.foldr (\{name, count} -> Map.insert name count) Map.empty res

        Map.lookup "1" mRes `shouldEqual` Just (embed $ -3 * 7 + -2 * 3 + 2 * 2)
        Map.lookup "2" mRes `shouldEqual` Just (embed $ 0 * 7 + -1 * 3 + 0 * 2)
        Map.lookup "3" mRes `shouldEqual` Just (embed $ 3 * 7 + 1 * 3 + -2 * 2)
        Map.size mRes `shouldEqual` 3
        pure unit
