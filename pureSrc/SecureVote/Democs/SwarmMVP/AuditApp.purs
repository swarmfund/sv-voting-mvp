module SecureVote.Democs.SwarmMVP.AuditApp where

import Prelude

import Control.Monad.Aff (Aff, throwError, error)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import Data.Array ((:))
import Data.Array as A
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Foldable (foldl)
import Data.Int (decimal, fromStringAs)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS)
import SecureVote.Democs.SwarmMVP.BallotContract (AllDetails, BallotResult, SUAux(..), StatusUpdate, ballotPropHelperAff, findEthBlockEndingInZeroBefore, makeErc20Contract, makeVotingContract, mkSUFail, mkSULog, mkSUSuccess, noArgs, runBallotCount, setWeb3Provider)
import SecureVote.Democs.SwarmMVP.Types (swmBallotShowJustVotes)
import SecureVote.Utils.Array (fromList)
import SecureVote.Utils.Decimal (toFixed)
import SecureVote.Utils.String (padLeft, padRight)


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


type AppArgs = {ethUrl :: String, ethRPCAuth :: String, votingAddr :: String, erc20Addr :: String }


-- | Main app function for Auditor. Accepts record of parameters needed to audit ballot.
app :: forall eff.
       AppArgs ->
       (StatusUpdate -> Unit) ->
       Aff (console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW, avar :: AVAR | eff) (Either (Tuple Int String) (Tuple Int BallotResult))
app {ethUrl, ethRPCAuth, votingAddr, erc20Addr} updateF =
    do
        setWeb3Provider ethUrl ethRPCAuth
        contract <- maybe (throwError $ error "Unable to instantiate voting contract") pure (makeVotingContract votingAddr)
        erc20Contract <- maybe (throwError $ error "Unable to instantiate erc20 contract") pure (makeErc20Contract erc20Addr)

        -- note: we actually fix the starting block number _not_ to the block immediately before or at the timestamp,
        -- but _the closest block number ending in zero_ before the timestamp. Basically it's a little quicker since
        -- there's no direct way to ask an ethereum node for this info (we have to search for it manually)
        startTimeS <- ballotPropHelperAff "startTime" noArgs contract
        -- this is a silly hack to avoid updateF triggering due to strict eval :/
        startTime <- case maybe (Left "Unable to parse ballot start time: ") Right (fromStringAs decimal startTimeS) of
            Right t -> pure t
            Left e -> const (pure 0) =<< (\_ -> updateFAff $ mkSUFail $ e <> startTimeS) =<< (throwError $ error $ e <> startTimeS)

        ballotAns <- runBallotCount startTime votingAddr contract erc20Contract {silent: false} updateF

        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = case ballotAns of
                Left err -> err
                Right (Tuple ballotResults allDeets) -> "Intricate Details:\n\n" <> formatAllDeets allDeets <> "\n\nSummary:\n\n" <> formatBallotResults ballotResults
        log $ "\n" <> msgStart <> "\n" <> msgBody

        let toRetE = case ballotAns of
                Right (Tuple ballotResult allDeets) -> (\_ -> Right ballotResult) $ updateF $ mkSUSuccess allDeets
                Left err -> (\_ -> Left err) $ updateF $ mkSUFail ("ERROR: " <> err)

        case toRetE of
            Right bRes -> pure $ Right $ Tuple exitC bRes
            Left err -> pure $ Left $ Tuple exitC err
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        updateFAff msg = (pure <<< updateF) =<< pure msg
