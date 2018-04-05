module Test.SV.SvLight.GetBallot where

import SV.Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Global.Unsafe (unsafeStringify)
import Network.Ethereum.Web3 (mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec)
import SV.Light.Types.Ballot (BallotSpec(..), OptsOuter(..))
import SecureVote.Web3.Web3 (EthNetwork(..), setNetwork)
import Test.SV.Types (SpecType)
import Test.Spec (it, pending)
import Test.Spec.Assertions (shouldEqual)


upmhx = unsafePartial fromJust <<< mkHexString
upma = unsafePartial fromJust <<< (mkAddress <=< mkHexString)


getBallotTests :: forall e. SpecType e
getBallotTests = do
    it "should be able to get ballot info" do
        setNetwork Kovan
        let bScAddr = upma "0x8a386981d1edf36c34c2d9e41f53ae016271f09a"
        log $ "Getting ballot info from SC: " <> show bScAddr
        bInfo <- getBallotInfo {bScAddr}  -- a test ballot on kovan
        log $ "Got ballot info from SC: " <> show bScAddr
        bInfo.bHash `shouldEqual` upmhx "0xd1866fa484dd79c95e1bb10e9974731b5c8e7c92237d64abe347c8598dbc8ee0"
        bInfo.startTime `shouldEqual` 1520155468
        bInfo.endTime `shouldEqual` 1521154943
        bInfo.encSecKey `shouldEqual` Nothing
        bInfo.creationBlock `shouldEqual` 6143074
        bInfo.startingBlockAround `shouldEqual` 6143074
        bInfo.nVotesCast `shouldEqual` 8

        bSpec <- getBallotSpec bInfo.bHash
        case bSpec of
            BVer01 bs -> do
                bs.startTime `shouldEqual` 1520154943
                bs.shortDesc `shouldEqual` "sd"
                bs.longDesc `shouldEqual` "ld"
                bs.options `shouldEqual` OptsBinary
                bs.discussionLink `shouldEqual` Nothing
                bs.erc20Addr `shouldEqual` upma "c3d10af066bde2357c92bc4af25fb5f42e73f1a4"
                bs.endTime `shouldEqual` 1521154943
                bs.encryptionPK `shouldEqual` Nothing
                bs.binding `shouldEqual` true
                bs.ballotTitle `shouldEqual` "d4-b1"
            -- _ -> throwError $ error "Unexpected BallotSpec version"

        pure unit

    pending "Another test"
