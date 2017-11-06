module SecureVote.Democs.SwarmMVP.Auditor where

import Prelude

import Control.Monad.Aff (Aff, Fiber, joinFiber, launchAff, runAff, throwError, error)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl.Types (NACL_RANDOM)
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.Yarn (leftpadBy, unlines)
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import Partial.Unsafe (unsafePartial)
import SecureVote.Democs.SwarmMVP.BallotContract (BallotResult, makeErc20Contract, makeSwmVotingContract, runBallotCount, setWeb3Provider)
import SecureVote.Democs.SwarmMVP.Const (balanceAt)
import SecureVote.Utils.Decimal (toFixed)
import SecureVote.Utils.String (padLeft, padRight)
import Unsafe.Coerce (unsafeCoerce)


formatBallotResults :: BallotResult -> String
formatBallotResults {winner, possibleWinners, totals} = resultsMsgHeader <> "\n" <> resultsMsgRest 
    where 
        resultsMsgHeader = case winner of
            (Just w) -> "Winner! \n" <> formatOpt w
            Nothing -> "Draw!!!\n " <> formatManyOpts possibleWinners
        resultsMsgRest = "\nTotals:\n" <> formatManyOpts totals
        formatManyOpts os = String.joinWith "\n" (map formatOpt os)
        formatOpt (Tuple opt nVotes) = padRight ' ' 30 opt <> " with # votes: " <> padLeft ' ' 30 decStr
            where
                decStr = toFixed 0 nVotes


app :: forall eff. String -> String -> String -> String -> Aff (console :: CONSOLE, naclRandom :: NACL_RANDOM, process :: PROCESS, now :: NOW | eff) Unit
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
                Right ballotResults -> formatBallotResults ballotResults
        log $ "\n" <> msgStart <> "\n"
        log $ msgBody
        liftEff $ exit exitC
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        

getArgs :: forall eff. String -> String -> String -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) {web3URI :: String, web3Auth :: String, addr :: String, erc20addr :: String}
getArgs web3URI web3Auth addr erc20addr = pure $ {web3URI, web3Auth, addr, erc20addr}


main = do
    let setup = usage "$0 --swmBallotAddr 0x1234abcd... --erc20Addr 0xabcd1234... [--ethnode <ethnodeUrl> [--ethnodeAuth <authDeets>]]"
                -- <> example "$0 "
                <> defaultHelp

    {web3URI, web3Auth, addr, erc20addr} <- runY setup $ getArgs 
                            <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                            <*> yarg "ethnodeAuth" [] (Just "username:password Auth for eth node if require") (Left "") true
                            <*> yarg "swmBallotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a voting contract address.") true
                            <*> yarg "erc20Addr" [] (Just "The Ethereum contract address of the ERC20 token") (Right "You must provide an erc20 contract address.") true

    launchAff do
        app web3URI web3Auth addr erc20addr

