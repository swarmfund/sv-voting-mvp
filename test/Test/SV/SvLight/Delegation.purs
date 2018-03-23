module Test.SV.SvLight.Delegation where

import SV.Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Console as A
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Array as Arr
import Data.Lens ((.~))
import Global.Unsafe (unsafeStringify)
import Network.Ethereum.Web3 (ChainCursor(..), _to, defaultTransactionOptions, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec)
import SV.Light.Delegation (findDelegatorsRecursive)
import SV.Light.SmartContract (mkSC)
import SV.Light.Types.Ballot (BallotSpec(..), OptsOuter(..))
import SV.Light.Types.RunBallot (SmartContract)
import SecureVote.Web3.Web3 (EthNetwork(..), setNetwork)
import Test.SV.Types (SpecType)
import Test.Spec (it, pending)
import Test.Spec.Assertions (shouldEqual)


upmhx = unsafePartial fromJust <<< mkHexString
upma = unsafePartial fromJust <<< (mkAddress <=< mkHexString)


delegationTests :: SpecType _
delegationTests = do
    it "should be able to resolve complex delegations (2 layers+)" cplxDlgtion
    pending "Another test"


cplxDlgtion :: Aff _ Unit
cplxDlgtion = do
        setNetwork Mainnet
        let swmTkn = upma "0x9e88613418cf03dca54d6a2cf6ad934a78c7a17a"
            sampleDelegate = upma "0xd56c6E89DA9F6cEab903878b98B1D91CfE1024D9"

        addrs <- findDelegatorsRecursive {tknAddr: swmTkn, delegate: sampleDelegate} dSc Latest
        (Arr.length addrs > 0) `shouldEqual` true

        A.log $ "Got delegators: " <> unsafeStringify addrs

        pure unit
  where
    dScAddr = upma "0x4dD28be042F85e287E9AaCe4147152bf1CD835e9"
    dSc :: forall args r e. SmartContract e args r
    dSc = mkSC $ (defaultTransactionOptions # _to .~ Just dScAddr)
