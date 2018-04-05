module SV.Light.Counts.RangeVote where

import SV.Prelude

import Data.Array as Arr
import Data.ByteString as BS
import Data.Foldable (foldl, foldr)
import Data.Int (binary, ceil, fromStringAs, toNumber)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Record as R
import Data.StrMap as StrMap
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (class Nat, class Pos, pred, reifyInt, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Math (log)
import Network.Ethereum.Web3 (BigNumber, HexString, embed)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import Partial.Unsafe (unsafePartial)
import SV.Light.Types.Ballot (SimpleOption(..))
import SV.Light.Types.RunBallot (GetVoteResult, BallotOptResult)
import SV.Utils.Binary (octetToBitStr)
import SV.Utils.Votes (nBitsForRangeVote)
import SecureVote.Utils.Array (chunk)
import SecureVote.Utils.Binary (hexStrToBs)


data RangeOffset = RangeAbsolute {rangeMax :: Int}
                 | RangePlusMinus {magnitude :: Int}


countRange :: RangeOffset -> Array SimpleOption -> Array GetVoteResult -> Array BallotOptResult
countRange offsetTyp opts weightedBallots = case offsetTyp of
    RangePlusMinus {magnitude} -> doCountRange {rangeMax: magnitude * 2, offset: magnitude}
    RangeAbsolute {rangeMax} -> doCountRange {rangeMax, offset: 0}
  where
    nOpts = Arr.length opts
    totalBal = foldr (+) (embed 0) (weightedBallots <#> \{bal} -> bal)
    -- The actual logic for counting ballots.
    -- We get each ballot and deserialize it which gives us a list of votes of length nOpts
    -- (note: the number of bits used for each vote opt is determined by rangeMax)
    -- we multiply each vote by the balance
    -- then we sum all votes into totals for their respective options & return
    doCountRange :: {rangeMax :: Int, offset :: Int} -> Array BallotOptResult
    doCountRange {rangeMax, offset} = procdBallots
                            # foldr addVecToOpts Map.empty
                            # Map.toUnfoldable
                            # map (\(Tuple i count) -> {name: getName i, count})
      where
        rawOpts = map unwrap opts
        names = R.get (SProxy :: SProxy "optionTitle") <$> rawOpts
        nameLookup = Map.fromFoldable $ Arr.zip (Arr.range 0 (nOpts - 1)) names
        getName i = unsafePartial fromJust $ Map.lookup i nameLookup
        procdBallots =
            weightedBallots
            -- pull ballots out from internal structure - HexString
            <#> (\{ballot: {ballot}, bal} -> {b: ballot, bal})
                >>> \{b, bal} -> {bal, vs: hexToRangeBallot {rangeMax, nOpts, offset} b}
        addVecToOpts :: {bal :: BigNumber, vs :: Array Int} -> Map.Map Int BigNumber -> Map.Map Int BigNumber
        addVecToOpts {bal, vs} m =
            -- multiply votes by balance
            map (embed >>> (*) bal) vs
            -- essentially zip with the index
            # Arr.mapWithIndex Tuple
            -- then add to existing entry in Map for that option number
            # foldr (\(Tuple i v) -> Map.alter (fromMaybe (embed 0) >>> (+) v >>> Just) i) m



hexToRangeBallot :: {rangeMax :: Int, nOpts :: Int, offset :: Int} -> HexString -> Array Int
hexToRangeBallot {rangeMax, nOpts, offset} rawBallot =
        -- chunk full bit string into vote bits
        String.toCharArray fullBitString  -- Array Char
        # chunk nBits                     -- Array (Array Char)
        # map String.fromCharArray        -- produces Array BitString
        -- take nOpts (dropping remainder)
        # Arr.take nOpts
        -- convert each to raw int vote
        # map (unsafePartial fromJust <<< fromStringAs binary)
        -- offset each vote
        # map (flip (-) offset)
  where
    nBits = nBitsForRangeVote rangeMax
    -- string of just ones and zeros
    fullBitString = rawBallot # hexStrToBs # BS.unpack # map octetToBitStr # foldr (<>) ""
