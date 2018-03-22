module Test.SV.CompleteCycle where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff (Aff, Canceler(..), Milliseconds(..), delay, error, makeAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxPublicKey, NACL_RANDOM, box, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Argonaut.Core as J
import Data.Array (drop, dropWhile, head, length, range, replicate, slice, tail, zip, (:))
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (toArray, toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Decimal (Decimal, fromInt)
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight)
import Data.Foreign (typeOf)
import Data.Int (decimal, fromNumber, fromString, fromStringAs, toStringAs)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Number as Num
import Data.Number.Format as NumF
import Data.Ord (abs)
import Data.String (joinWith, take)
import Data.String.Utils (lines, words)
import Data.Traversable (and, sequence)
import Data.Tuple (Tuple(..))
import Data.TypedArray (asUint8Array)
import Data.TypedArray as TA
import Node.Buffer (Buffer)
import Node.Buffer as B
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv, PROCESS)
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toNonce, toBoxPubkey, toMessage, encryptOneTimeBallot)
import SecureVote.Crypto.NativeEd25519 (sha256)
import SecureVote.Democs.SwarmMVP.AuditApp (formatBallotResults)
import SecureVote.Democs.SwarmMVP.Ballot (makeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (AllDetails, BallotResult, EncBallotWVoter, Erc20Contract(..), SwmVotingContract(..), ballotPropHelper, ballotPropHelperAff, ballotPropHelperWBlockNumAff, getAccount, getBallotEncPK, getBlockNumber, getBlockTimestamp, makeErc20Contract, makeVotingContract, noArgs, releaseSecretKey, runBallotCount, setBallotEndTime, setWeb3Provider, web3CastBallot)
import SecureVote.Democs.SwarmMVP.KeyGen (generateKey)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex)
import SecureVote.Utils.Numbers (intToStr)
import SecureVote.Utils.Time (currentTimestamp)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)


type AffAll e a = Aff (cp :: CHILD_PROCESS, console :: CONSOLE, now :: NOW, naclRandom :: NACL_RANDOM, random :: RANDOM | e) a


rpcPort :: Int
rpcPort = 8545

rpcPortStr :: String
rpcPortStr = toStringAs decimal rpcPort


-- Don't set this to more than 200 (we generate 210 accounts in testrpc during the auto-tests)
-- Note: TestRPC seems to break between the 500 and 700 mark (500 works).
nVotes :: Int
nVotes = 200

extraVotes :: Int
extraVotes = 5


logBuffer str = unsafePerformEff $ B.toString UTF8 str

logUC = log <<< unsafeCoerce

self :: forall a. a -> a
self a = a

unsFromJ :: forall a. Maybe a -> a
unsFromJ a = unsafePartial $ fromJust a


waitTime :: Number
waitTime = 60.0


optionNames :: Array String
optionNames = ["opt1", "opt2", "option name 3 which is much longer than length 32", "opt4", ""]


completeBallotTest :: forall e. SpecType (e)
completeBallotTest = do
    it "should compile and deploy the contract, cast randomised votes, retrived and decrypt them, and count those votes correctly." do
        -- setup rpc server
        -- testRpc <- testRpcServer rpcPort
        setWeb3Provider ("http://localhost:" <> rpcPortStr) ""
        let voterIds = range 1 (nVotes + 1)

        -- compile contract
        compileOut <- compileSol []
        log "Compiled contract"

        compileErc20Out <- compileSol ["-c", "Erc20.sol"]
        log "Compiled Erc20 contract"

        -- deploy it
        {pk: encPk, sk: encSk, outBuffer: deployOut} <- deploySol
        let ui8SK = unsafePartial $ fromJust $ fromHex encSk
        let ui8PK = unsafePartial $ fromJust $ fromHex encPk
        deployStr <- liftEff $ B.toString UTF8 deployOut
        log $ "Deployed voting contract."

        deployErc20Out <- deployArbitrary "Erc20"
        deployErc20Str <- liftEff $ B.toString UTF8 deployErc20Out
        log $ "Deployed test Erc20."

        -- get contract object
        let addrM = contractAddrM deployStr
        let contractM = outputToVotingContract addrM
        Tuple contract votingAddr <- maybe (throwError $ error "Unable to get voting contract") pure $ lift2 Tuple contractM addrM
        log $ "Voting contract deployed at: " <> (unsafePartial $ fromJust addrM)

        -- check owner
        votingOwner <- ballotPropHelperAff "owner" [] contract
        log $ "Got owner: " <> votingOwner
        coinbase <- either (throwError <<< error) pure (getAccount 0)
        votingOwner `shouldEqual` coinbase
        log $ "Voting owner == coinbase"

        -- do erc20
        let erc20AddrM = contractAddrM deployErc20Str
        erc20Contract <- maybe (throwError $ error "erc20 contract was not created") pure (erc20AddrM >>= makeErc20Contract)
        log $ "Erc20 deployed at: " <> (unsFromJ erc20AddrM)
        erc20Owner <- ballotPropHelperAff "owner" [] erc20Contract
        erc20Owner `shouldEqual` coinbase

        -- give all voters some coins
        log $ "Sending ERC20 tokens to users"
        balances <- genBalances voterIds
        log "Generated balances for each user, sending..."
        setBalanceTxs <- parTraverse (\{addr, newBal} -> ballotPropHelperAff "transfer" [addr, Dec.toString newBal] erc20Contract) balances
        log $ "Distributed ERC20 tokens; verifying balances now..."
        balancesMatch <- checkBalances erc20Contract balances
        balancesMatch `shouldEqual` true
        log $ "ERC20 distribution complete and accurate."

        -- get current block number for later (before we cast ballots)
        blockNum <- getBlockNumber
        balanceTs <- getBlockTimestamp blockNum
        log $ "Block number: " <> show blockNum

        -- create lots of ballots
        ballots <- createBallots nVotes contractM
        log $ "5 sample ballots: " <> (joinWith ", " $ map toHex $ Array.take 5 ballots)

        let encdBallots = encryptBallots (ui8PK) ballots
        log $ "Encrypted " <> (toStringAs decimal $ length encdBallots) <> " ballots."
        nVotes `shouldEqual` (length encdBallots)

        -- publish ballots
        let enumeratedBallots = zip voterIds encdBallots
        log "Casting ballots"
        ballotTxids <- castBallots enumeratedBallots contractM
        logUC $ head ballotTxids

        -- cast the first `extraVotes` ballots again -- doesn't matter that they're the same
        -- only thing that matters is that they're removed
        log $ "Casting " <> intToStr extraVotes <> " duplicate ballots"
        dupBallotTxids <- castBallots (Array.take extraVotes enumeratedBallots) contractM

        -- end the ballot or wait depending on ENV variables
        let shouldWaitStr = unsafePerformEff $ (lookupEnv "SV_MANUAL_TEST")
        let (shouldWait :: Boolean) = shouldWaitStr == Just "true"
        if shouldWait
            then
                log "Waiting for 90s for ballot to finish and manual testing..."
                *> delay (Milliseconds $ 1000.0 * waitTime)
            else
                log "Setting endTime to the past"
                *> setBallotEndTime 1508822279 contract  -- corresponds to 2017/10/24 5:17:58 UTC
                >>= logUC

        -- release secret key
        log $ "Releasing Secret Key:"
        releaseSKTxid <- releaseSecretKey encSk contract
        logUC releaseSKTxid

        -- count ballot
        ballotResultE :: Either String _ <- runBallotCount balanceTs votingAddr contract erc20Contract {silent: false} (\_ -> unit)

        -- check count results
        ballotSuccess <- logAndPrintResults ballotResultE
        ballotSuccess `shouldEqual` true

        let (nVotesInContract :: Int) = unsFromJ $ (fromStringAs decimal) $ unsafePartial $ fromRight $ ballotPropHelper "nVotesCast" noArgs contract
        if shouldWait then
                (nVotesInContract >= (nVotes + extraVotes)) `shouldEqual` true
            else
                nVotesInContract `shouldEqual` (nVotes + extraVotes)

        pure unit
  where
    contractAddrM outputStr = Just outputStr >>= extractContractAddr
    outputToVotingContract addrM = addrM >>= makeVotingContract
    getEncPk cM = do
        contract <- maybe (Left "No contract") Right cM
        getBallotEncPK contract


compileSol :: forall e. Array String -> AffAll e Buffer
compileSol args = do
        affExec "./bin/solidity/compile.sh" args


deploySol :: forall e. AffAll e {pk :: String, sk :: String, outBuffer :: Buffer}
deploySol = do
        nowTime <- liftEff currentTimestamp
        let endTime = NumF.toString $ nowTime + waitTime
        {sk, pk} <- liftEff generateKey
        outBuffer <- affExec "node" ["./bin/solidity/deploy.js", "--startTime", "0",
                "--endTime", endTime, "--ballotEncPubkey", "0x" <> pk,
                "--unsafeSkipChecks", "--deploy", "--web3Provider",
                "http://localhost:" <> rpcPortStr, "--testing", "--optionNamesJson", optionNamesJson]
        log $ "Generated Encryption PublicKey : " <> pk
        log $ "Generated Encryption SecretKey : " <> sk
        pure $ {pk, sk, outBuffer}
  where
    optionNamesJson = J.stringify $ J.fromArray $ J.fromString <$> optionNames


deployArbitrary :: forall e. String -> AffAll e Buffer
deployArbitrary contractName = do
        outBuffer <- affExec "node" ["./bin/solidity/deploy.js", "--startTime", "0",
                "--endTime", "0", "--ballotEncPubkey", "0x00",
                "--unsafeSkipChecks", "--deploy", "--web3Provider",
                "http://localhost:" <> rpcPortStr, "--deployOther", contractName]
        pure outBuffer


extractContractAddr :: String -> Maybe String
extractContractAddr output = addr
    where
      lineM = head $ dropWhile (not <<< lineMatches) $ lines output
      lineMatches str = "Contract Addr: 0x" == take 17 str
      addr = lineM >>= (\str' -> head $ drop 2 $ words str')


type Balance = {addr :: String, newBal :: Decimal}
genBalances :: forall e. Array Int -> AffAll e (Array Balance)
genBalances voterIds = parTraverse genBal voterIds
    where
        dps9 = Dec.fromInt 1000000000
        dps18 = dps9 * dps9
        genBal voterId = do
            giveCoins <- liftEff $ randomInt 1 10  -- random in [1, 10]
            -- 80% chance to get coins
            newBal <- ((*) dps18) <$> (fromInt <$> if giveCoins <= 8 then liftEff $ randomInt 1000 1000000 else pure 0)
            addr <- either (throwError <<< error) pure (getAccount voterId)
            pure $ {addr, newBal}


checkBalances :: forall e. Erc20Contract -> Array Balance -> AffAll e Boolean
checkBalances erc20 balances = do
        balanceChecks <- parTraverse checkBal balances
        pure $ and balanceChecks
    where
        checkBal {addr, newBal} = do
            balStr <- ballotPropHelperAff "balanceOf" [addr] erc20
            let bal = Dec.fromString balStr
            let areEq = (bal == Just newBal) || (balStr == "0" && newBal == Dec.fromInt 0)
            if not areEq then
                    log $ "Err: balances not equal: " <> show newBal <> " and (" <> show bal <> ")"
                else
                    pure unit
            pure areEq

createBallots :: forall e. Int -> Maybe SwmVotingContract -> AffAll e ((Array Uint8Array))
createBallots nVotes cM = case cM of
    Nothing -> throwError $ error "Contract is Nothing!"
    Just contract -> do
        let encPkM = getBallotEncPK contract
        case encPkM of
            Left _ -> throwError $ error "could not get encryption pubkey from contract"
            Right encPk -> do
                liftEff $ genBallots nVotes


genBallots :: forall e. Int -> Eff (random :: RANDOM | e) (Array Uint8Array)
genBallots 0 = pure []
genBallots n = do
    setDelegate <- randomInt 0 2  -- 1/3 chance to NOT delegate
    delegateE <- if setDelegate /= 0 then getAccount <$> randomInt 1 nVotes else pure $ Right "0x1111122222333334444411111222223333344444"
    let (delegate :: String) = unsafePartial $ fromRight delegateE
    ballot <- makeBallot delegate
    ballots <- genBallots (n-1)
    pure $ ballot : ballots


encryptBallots :: forall e. Uint8Array -> Array Uint8Array -> (Array EncBallotWVoter)
encryptBallots _ [] = []
encryptBallots encPk ballots = do
        ballot <- ballots
        let keypair = unsafePerformEff $ genCurve25519Key
        let voterPk = getBoxPublicKey keypair
        let voterSk = getBoxSecretKey keypair
        let encBallot = toUint8Array $ encryptOneTimeBallot voterPk (toMessage ballot) (toBoxPubkey encPk) voterSk
        pure $ {encBallot, voterPk}


castBallots :: forall e. Array (Tuple Int EncBallotWVoter) -> Maybe SwmVotingContract -> Aff (| e) (Array String)
castBallots _ Nothing = pure []
castBallots [] _ = pure []
castBallots allBallots (Just contract) = do
        parTraverse castBallot allBallots
    where
        castBallot (Tuple i ballotPack) = web3CastBallot i ballotPack contract


logAndPrintResults :: forall e. Either String (Tuple BallotResult AllDetails) -> Aff (console :: CONSOLE | e) Boolean
logAndPrintResults ballotResults = case ballotResults of
    Left err -> do
        log $ "Ballot Count Failed!\n" <> err
        pure false
    Right (Tuple res allResults) -> do
        log $ "Ballot count success!"
        log $ formatBallotResults res
        pure true



affExec :: forall eff. String -> Array String -> Aff (cp :: CHILD_PROCESS | eff) Buffer
affExec cmd args = makeAff $ \cb -> do
    execFile cmd args defaultExecOptions $ \ex -> cb $ maybe (Right ex.stdout) Left ex.error
    pure $ Canceler (\_ -> do
        _ <- throwError $ error $ "Canceled: " <> cmd <> (joinWith " " args)
        pure unit)
