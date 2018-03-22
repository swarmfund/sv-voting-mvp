module SV.AuditCLI where


import SV.Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Crypt.NaCl (NACL_RANDOM)
import IPFS (IPFSEff)
import Network.Ethereum.Web3 (ETH)
import Node.Buffer (BUFFER)
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (flag, runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import SV.Light.AuditApp (AppArgs, app)


getArgs :: forall eff. String -> String -> Boolean -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) AppArgs
getArgs e a d = pure $ {ethUrl: e, bScAddr: a, dev: d}


main :: forall e. Eff ( eth :: ETH, exception :: EXCEPTION, console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW, process :: PROCESS, avar :: AVAR, ref :: REF, buffer :: BUFFER, ipfs :: IPFSEff | e) Unit
main = do
    let setup = usage "$0 --bScAddr 0x1234abcd... [--ethUrl <ethNodeUrl>] [--dev <boolean>]"
                <> defaultHelp

    args <- runY setup $ getArgs
                            <$> yarg "e" ["ethNode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.eth.secure.vote:8545/ballotAudit") true
                            <*> yarg "bScAddr" [] (Just "The Ethereum contract address of the ballot box") (Right "The Ballot Box Smart Contract address is required.") true
                            <*> flag "dev" [] (Just "Whether to use dev mode")

    launchAff_ do
        resE <- app args (\_ -> unit)
        liftEff $ exit $ case resE of
                Right (Tuple ec _) -> ec
                Left (Tuple ec _) -> ec
