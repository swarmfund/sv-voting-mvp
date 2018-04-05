module SV.Light.Counts.BinaryVote where

import SV.Light.Types.RunBallot
import SV.Prelude

import Data.Foldable (foldr)
import Data.Map as Map
import Network.Ethereum.Web3 (mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Utils.BigNumber (bnFromMDef0)


countBinary :: Array GetVoteResult -> Array BallotOptResult
countBinary weightedBallots =
    let resultsMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (bnFromMDef0 >>> (+) bal >>> Just) ballot m)
                        Map.empty weightedBallots
        ballotYes = unsafePartial fromJust $ mkHexString "8000000000000000000000000000000000000000000000000000000000000000"
        ballotNo = unsafePartial fromJust $ mkHexString "4000000000000000000000000000000000000000000000000000000000000000"
    in
    [ {name: "yes", count: bnFromMDef0 $ Map.lookup ballotYes resultsMap}
    , {name: "no", count: bnFromMDef0 $ Map.lookup ballotNo resultsMap}
    ]
