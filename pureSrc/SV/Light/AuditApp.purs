module SV.Light.AuditApp where

import SV.Prelude

import Control.Monad.Aff (Aff, throwError, error)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExceptT)
import Crypt.NaCl (NACL_RANDOM)
import Data.Array (foldr, (:))
import Data.Array as A
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Foldable (foldl)
import Data.Int (decimal, fromStringAs)
import Data.Lens ((.~), (^.))
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap as StrMap
import Data.String as String
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3 (_to, defaultTransactionOptions, mkAddress, mkHexString)
import Network.Ethereum.Web3 as BN
import Node.Process (PROCESS)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec, runBallotCount)
import SV.Light.Delegation (dlgtAddr)
import SV.Types.Lenses (_erc20Addr)
import SV.Types.OutboundLogs (StatusUpdate, mkSUFail, mkSULog, mkSUSuccess)
import SV.Utils.BigNumber (bnToStr)
import SecureVote.Democs.SwarmMVP.BallotContract (AllDetails, BallotResult, findEthBlockEndingInZeroBefore, noArgs)
import SecureVote.Democs.SwarmMVP.Types (swmBallotShowJustVotes)
import SecureVote.Utils.Array (fromList)
import SecureVote.Utils.Decimal (toFixed)
import SecureVote.Utils.String (padLeft, padRight)
import SecureVote.Web3.Web3 (setProvider)


formatBallotResults :: BallotResult -> String
formatBallotResults {winner, possibleWinners, totals} = resultsMsgHeader <> resultsMsgRest
    where
        resultsMsgHeader = case winner of
            (Just w) -> "Winner! \n" <> formatOpt w
            Nothing -> "Draw!!!\n " <> formatManyOpts possibleWinners
        resultsMsgRest = "\nTotals:\n" <> formatManyOpts totals
        formatManyOpts os = String.joinWith "\n" (map formatOpt os)
        formatOpt (Tuple opt nVotes) = padRight ' ' 30 opt <> " with # votes: " <> padLeft ' ' 30 decStr
            where
                decStr = toFixed 0 nVotes


formatAllDeets :: AllDetails -> String
formatAllDeets {encBallotsWithoutDupes, decryptedBallots, delegateMapNoLoops, ballotMap, balanceMap} =
    formattedResponse
        where
            formattedResponse = String.joinWith "\n" (makeCsvRow rowTitles : csvRows)
            csvRows = map makeCsvRow votersWDetails
            makeCsvRow d = String.joinWith "," [d.voter, d.delegate, d.vote, d.balance]
            votersWDetails = map getVoterDetails voters
            rowTitles = {voter: "Voter", delegate: "Delegate", vote: "Vote", balance: "Balance"}
            getVoterDetails voter =
                { voter: show voter
                , delegate: fromMaybe "No Delegate" $ show <$> (join $ lookup voter delegateMapNoLoops)
                , vote: show $ fromMaybe "Error: no ballot to get" $ either (\err -> "Error: " <> err) swmBallotShowJustVotes <$> lookup voter ballotMap
                , balance: show $ fromMaybe "Error: no balance found" $ toFixed 2 <$> lookup voter balanceMap
                }
            voters = fromList $ Map.keys balanceMap


type AppArgs = {ethUrl :: String, bScAddr :: String, dev :: Boolean}


-- | Main app function for Auditor. Accepts record of parameters needed to audit ballot.
app :: forall eff.
       AppArgs ->
       (StatusUpdate -> Unit) ->
       Aff _ _ -- (Either (Tuple Int String) (Tuple Int BallotResult))
app {ethUrl, bScAddr, dev} updateF =
    do
        if dev then log "-- DEV MODE --" else pure unit
        liftEff $ setProvider ethUrl
        addr <- mToAff "Unable to get BallotBox SC address" $ mkAddress =<< mkHexString bScAddr
        bInfo <- getBallotInfo {bScAddr: addr}
        bSpec <- getBallotSpec bInfo.bHash
        let bbTos = defaultTransactionOptions # _to .~ Just addr
            ercTos = defaultTransactionOptions # _to .~ Just (bSpec ^. _erc20Addr)
            dlgtTos = defaultTransactionOptions # _to .~ Just (dlgtAddr {dev})

        ballotAns <- runExceptT $ runBallotCount {bInfo, bSpec, bbTos, ercTos, dlgtTos, silent: false} updateF

        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = case ballotAns of
                Left err -> err
                Right {ballotResults: bRes} -> "\n\nResults:\n"
                        <> foldr (\{name, count} rem -> "\n" <> name <> ": " <> bnToStr count) "" bRes
        log $ "\n" <> msgStart <> "\n" <> msgBody

        let toRetE = case ballotAns of
                Right bRes@{nVotes, ballotResults} ->
                    (\_ -> Right bRes) $ updateF $ mkSUSuccess {nVotes, ballotResults: mkBResStrMap ballotResults}
                Left err -> (\_ -> Left err) $ updateF $ mkSUFail ("ERROR: " <> err)

        case toRetE of
            Right bRes -> pure $ Right $ Tuple exitC bRes
            Left err -> pure $ Left $ Tuple exitC err
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        mkBResStrMap bRes = StrMap.fromFoldable $ (\{name, count} -> Tuple name count) <$> bRes
