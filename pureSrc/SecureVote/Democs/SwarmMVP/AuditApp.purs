module SecureVote.Democs.SwarmMVP.AuditApp where

import Prelude

import Control.Monad.Aff (Aff, throwError, error)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import Data.Array ((:))
import Data.Array as A
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Foldable (foldl)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS)
import SecureVote.Democs.SwarmMVP.BallotContract (BallotResult, AllDetails, makeErc20Contract, makeSwmVotingContract, runBallotCount, setWeb3Provider)
import SecureVote.Democs.SwarmMVP.Const (balanceAt)
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


app :: forall eff. String -> String -> String -> String -> Aff (console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW | eff) Int
app ethUrl ethNodeAuth swmAddress erc20Address =
    do
        let _ = setWeb3Provider ethUrl ethNodeAuth
        contract <- maybe (throwError $ error "Unable to instantiate voting contract") pure (makeSwmVotingContract swmAddress)
        erc20Contract <- maybe (throwError $ error "Unable to instantiate erc20 contract") pure (makeErc20Contract erc20Address)
        ballotAns <- runBallotCount balanceAt contract erc20Contract {silent: false}
        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = case ballotAns of
                Left err -> err
                Right (Tuple ballotResults allDeets) -> "Intricate Details:\n\n" <> formatAllDeets allDeets <> "\n\nSummary:\n\n" <> formatBallotResults ballotResults
        log $ "\n" <> msgStart <> "\n"
        log $ msgBody
        pure exitC
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
