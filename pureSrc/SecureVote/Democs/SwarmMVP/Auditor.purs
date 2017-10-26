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
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Maybe (Maybe(..), maybe)
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import Partial.Unsafe (unsafePartial)
import SecureVote.Democs.SwarmMVP.BallotContract (makeErc20Contract, makeSwmVotingContract, runBallotCount, setWeb3Provider)
import Unsafe.Coerce (unsafeCoerce)


app :: forall eff. String -> String -> String -> Aff (console :: CONSOLE, naclRandom :: NACL_RANDOM, process :: PROCESS, now :: NOW | eff) Unit
app ethUrl swmAddress erc20Address = 
    do
        let _ = setWeb3Provider ethUrl
        contract <- maybe (throwError $ error "Unable to instantiate voting contract") pure (makeSwmVotingContract swmAddress)
        erc20Contract <- maybe (throwError $ error "Unable to instantiate erc20 contract") pure (makeErc20Contract erc20Address)
        ballotAns <- runBallotCount contract erc20Contract {silent: false}
        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = unsafeCoerce $ unsafePartial $ fromRight ballotAns
        log $ "\n" <> msgStart <> "\n"
        log $ msgBody
        liftEff $ exit exitC
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        

getArgs :: forall eff. String -> String -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) {web3URI :: String, addr :: String, erc20addr :: String}
getArgs web3URI addr erc20addr = pure $ {web3URI, addr, erc20addr}


main = do
    let setup = usage "$0 --swmBallotAddr 0x1234abcd... [--ethnode <ethnodeUrl>]"
                -- <> example "$0 "
                <> defaultHelp

    {web3URI, addr, erc20addr} <- runY setup $ getArgs 
                            <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                            <*> yarg "swmBallotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a voting contract address.") true
                            <*> yarg "erc20Addr" [] (Just "The Ethereum contract address of the ERC20 token") (Right "You must provide an erc20 contract address.") true

    launchAff do
        app web3URI addr erc20addr

