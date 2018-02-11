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
import Data.Tuple (Tuple(..))
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import SecureVote.Democs.SwarmMVP.AuditApp (app, AppArgs)


getArgs :: forall eff. String -> String -> String -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) AppArgs
getArgs ethUrl ethRPCAuth votingAddr erc20Addr = pure $ {ethUrl, ethRPCAuth, votingAddr, erc20Addr}


main :: forall e. Eff ( exception :: EXCEPTION, console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW, process :: PROCESS | e) Unit
main = do
    let setup = usage "$0 --ballotAddr 0x1234abcd... --erc20Addr 0xabcd1234... [--ethUrl <ethNodeUrl> [--ethRPCAuth <authDeets>]]"
                <> defaultHelp

    args <- runY setup $ getArgs
                            <$> yarg "e" ["ethNode"] (Just "HTTP URL of Eth node") (Left "http://eth-aws-nv-node-02.secure.vote:38545/ballotAudit") true
                            <*> yarg "ethRPCAuth" [] (Just "username:password Auth for eth node if require, default: '' (an empty string)") (Left "") true
                            <*> yarg "ballotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a voting contract address.") true
                            <*> yarg "erc20Addr" [] (Just "The Ethereum contract address of the ERC20 token") (Left "0x9e88613418cf03dca54d6a2cf6ad934a78c7a17a") true

    launchAff_ do
        resE <- app args (\_ -> unit)
        liftEff $ exit $ case resE of
                Right (Tuple ec _) -> ec
                Left (Tuple ec _) -> ec
