module SecureVote.Democs.SwarmMVP.Auditor where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Crypt.NaCl.Types (NACL_RANDOM)
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe(..))
import Node.Process (PROCESS, exit) 
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import SecureVote.Democs.SwarmMVP.BallotContract (makeSwmVotingContract, runBallotCount, setWeb3Provider)
import Unsafe.Coerce (unsafeCoerce)


app :: forall eff. String -> String -> Eff (console :: CONSOLE, naclRandom :: NACL_RANDOM, process :: PROCESS | eff) Unit
app ethUrl swmAddress = 
    do
        let a = setWeb3Provider ethUrl
        let mContract = makeSwmVotingContract swmAddress
        let ballotAns = runBallotCount mContract
        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = either id id ballotAns
        log $ "\n" <> msgStart <> "\n"
        log $ msgBody
        exit exitC
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        


main :: forall eff. Eff (console :: CONSOLE, exception :: EXCEPTION, naclRandom :: NACL_RANDOM, process :: PROCESS | eff) Unit
main = do
    let setup = usage "$0 --swmBallotAddr 0x1234abcd... [--ethnode <ethnodeUrl>]"
                -- <> example "$0 "
                <> defaultHelp

    runY setup $ app <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                    <*> yarg "swmBallotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a contract address.") true


