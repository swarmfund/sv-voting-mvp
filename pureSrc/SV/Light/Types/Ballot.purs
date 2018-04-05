module SV.Light.Types.Ballot where

import SV.Prelude

import Data.Array (length)
import Data.Foreign (Foreign)
import Data.Newtype (class Newtype)
import Data.Record.ShowRecord (showRecord)
import Global.Unsafe (unsafeStringify)
import Network.Ethereum.Web3.Types.Types (Address, HexString, mkAddress, mkHexString)
import SecureVote.Utils.Json (mkFErr)
import Simple.JSON (class ReadForeign, read')


data BallotSpec
    = BVer01 BSpec01Impl


instance showBSpec :: Show BallotSpec where
    show bs = case bs of
        BVer01 a -> "( BallotSpec Version 01 => " <> showRecord a <> " )"


data BallotSpecChoice
    = BChoice01


bSpecChoiceToStr :: BallotSpecChoice -> String
bSpecChoiceToStr c =
    case c of
        BChoice01 -> "Standard Ballot (v01)"


getTitle :: BallotSpec -> String
getTitle b =
    case b of
        BVer01 d -> d.ballotTitle


type BSpec01Impl =
    { ballotTitle :: String
    , shortDesc :: String
    , longDesc :: String
    , startTime :: Int
    , endTime :: Int
    , erc20Addr :: Address
    , discussionLink :: Maybe String
    , binding :: Boolean
    , encryptionPK :: Maybe HexString
    , options :: OptsOuter
    }


data OptsOuter
    = OptsSimple SimpleVer (Array SimpleOption)
    | OptsBinary


derive instance eqOptsOuter :: Eq OptsOuter
instance showOptsOuter :: Show OptsOuter where
    show oo = (\s -> "( OptsOuter " <> s <> " )") $ case oo of
        OptsBinary -> "OptsBinary"
        OptsSimple v os -> (\s -> "( OptsSimple " <> s <> " )") $ case v of
            RangeVotingPlusMinus3 -> "RangeVotingPlusMinus3 ( " <> show (showSimpleOpt <$> os) <> " )"


optsNOptions :: OptsOuter -> Int
optsNOptions os =
    case os of
        OptsSimple RangeVotingPlusMinus3 xs -> length xs
        OptsBinary -> 1


data OptsChoice
    = OChSimpleRange
    | OChBinary


oChoiceToStr :: OptsChoice -> String
oChoiceToStr o =
    case o of
        OChSimpleRange -> "Range Voting"
        OChBinary -> "Binary Yes/No"


newtype SimpleOption = SimpleOption
    { optionTitle :: String, optionDesc :: Maybe String }

derive instance eqSimpleOption :: Eq SimpleOption
derive instance ntSimpleOption :: Newtype SimpleOption _

showSimpleOpt :: SimpleOption -> String
showSimpleOpt (SimpleOption {optionTitle, optionDesc}) = "[ Title: " <> optionTitle <> ", Desc: " <> show optionDesc <> " ]"


data SimpleVer
    = RangeVotingPlusMinus3

derive instance eqSimpleV :: Eq SimpleVer

type ReadBSpecStage1 = { ballotVersion :: Int, ballotInner :: Foreign }

type ReadOptsOuterStage1 = { optionsVersion :: Int, options :: Maybe Foreign }


instance readFBallotSpec :: ReadForeign BallotSpec where
    readImpl a = do
        (s1 :: ReadBSpecStage1) <- read' a
        case s1.ballotVersion of
            1 -> b01Conv =<< read' s1.ballotInner
            _ -> mkFErr $ "Invalid BallotSpec: " <> unsafeStringify s1.ballotInner
      where
        b01Conv b@{encryptionPK, erc20Addr} = do
            (erc_ :: Address) <- fromMaybe (mkFErr "Cannot convert erc20Addr to addr") (pure <$> (mkAddress =<< mkHexString =<< erc20Addr :: Maybe String))
            -- go from Maybe String -> Maybe Maybe HexString -> F (Maybe HexString)
            (encPk_ :: Maybe HexString) <- case encryptionPK of
                Nothing -> pure Nothing
                Just ePK -> fromMaybe (mkFErr "Cannot convert erc20Addr to addr") (map (pure <<< Just) $ mkHexString ePK :: Maybe HexString)
            pure $ BVer01 $ b {encryptionPK = encPk_, erc20Addr = erc_}
        mkExcept = pure


instance readFOptsOuter :: ReadForeign OptsOuter where
    readImpl a = do
        (s1 :: ReadOptsOuterStage1) <- read' a
        case s1.optionsVersion of
            1 -> opt01Conv =<< read' =<< (fromMaybe (mkFErr "OptionsV01 expected SimpleOptions but got Nothing") (pure <$> s1.options))
            2 -> if isJust s1.options then mkFErr $ "Options Binary expected nothing for `options` but got: " <> unsafeStringify s1.options else pure (opt02Conv s1)
            _ -> mkFErr $ "Invalid Options in BallotSpec: " <> unsafeStringify s1.options
      where
        opt01Conv options = do
            opts :: Array SimpleOption <- read' options
            pure $ OptsSimple RangeVotingPlusMinus3 opts
        opt02Conv o = OptsBinary


instance readSimpleOption :: ReadForeign SimpleOption where
    readImpl a = do
        SimpleOption <$> read' a
