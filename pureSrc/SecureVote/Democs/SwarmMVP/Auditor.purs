module SecureVote.Democs.SwarmMVP.Auditor where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import SecureVote.Democs.SwarmMVP.AuditApp (app)


getArgs :: forall eff. String -> String -> String -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) {web3URI :: String, web3Auth :: String, addr :: String, erc20addr :: String}
getArgs web3URI web3Auth addr erc20addr = pure $ {web3URI, web3Auth, addr, erc20addr}


main :: forall e. Eff ( exception :: EXCEPTION, console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW, process :: PROCESS | e) Unit
main = do
    let setup = usage "$0 --swmBallotAddr 0x1234abcd... --erc20Addr 0xabcd1234... [--ethnode <ethnodeUrl> [--ethnodeAuth <authDeets>]]"
                -- <> example "$0 "
                <> defaultHelp

    {web3URI, web3Auth, addr, erc20addr} <- runY setup $ getArgs
                            <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                            <*> yarg "ethnodeAuth" [] (Just "username:password Auth for eth node if require") (Left "") true
                            <*> yarg "swmBallotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a voting contract address.") true
                            <*> yarg "erc20Addr" [] (Just "The Ethereum contract address of the ERC20 token") (Left "0x9e88613418cf03dca54d6a2cf6ad934a78c7a17a") true

    launchAff_ do
        exitCode <- app web3URI web3Auth addr erc20addr
        liftEff $ exit exitCode
