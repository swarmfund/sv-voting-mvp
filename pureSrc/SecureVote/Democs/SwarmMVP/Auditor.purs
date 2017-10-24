module SecureVote.Democs.SwarmMVP.Auditor where

import Prelude

import Control.Monad.Aff (Aff, Fiber, joinFiber, launchAff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl.Types (NACL_RANDOM)
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe(..))
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (defaultHelp, usage)
import SecureVote.Democs.SwarmMVP.BallotContract (makeSwmVotingContract, runBallotCount, setWeb3Provider)
import Unsafe.Coerce (unsafeCoerce)


app :: forall eff. String -> String -> Aff (console :: CONSOLE, naclRandom :: NACL_RANDOM, process :: PROCESS, now :: NOW | eff) Unit
app ethUrl swmAddress = 
    do
        let a = setWeb3Provider ethUrl
        let mContract = makeSwmVotingContract swmAddress
        ballotAns <- runBallotCount mContract
        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = unsafeCoerce $ ballotAns
        log $ "\n" <> msgStart <> "\n"
        log $ msgBody
        liftEff $ exit exitC
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then "___Success:___" else ">>> ERROR <<<"
        

getArgs :: forall eff. String -> String -> Eff (console :: CONSOLE, exception :: EXCEPTION | eff) {web3URI :: String, addr :: String}
getArgs web3URI addr = pure $ {web3URI, addr}


main = do
    let setup = usage "$0 --swmBallotAddr 0x1234abcd... [--ethnode <ethnodeUrl>]"
                -- <> example "$0 "
                <> defaultHelp

    {web3URI, addr} <- runY setup $ getArgs 
                            <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                            <*> yarg "swmBallotAddr" [] (Just "The Ethereum contract address of the Swarm ballot") (Right "You must provide a contract address.") true

    launchAff do
        app web3URI addr

