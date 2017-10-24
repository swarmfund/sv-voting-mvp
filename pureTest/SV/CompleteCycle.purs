module Test.SV.CompleteCycle where
  
import Prelude

import Control.Monad.Aff (Aff, Canceler(..), error, makeAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxPublicKey, NACL_RANDOM, box, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Array (drop, dropWhile, head, length, range, replicate, slice, tail, zip, (:))
import Data.Array as Array
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (toArray, toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..), either, fromRight)
import Data.Int (decimal, fromNumber, fromString, fromStringAs, toStringAs)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Number as Num
import Data.Number.Format (toString)
import Data.Ord (abs)
import Data.String (joinWith)
import Data.String.CodePoints (take)
import Data.String.Utils (lines, words)
import Data.Traversable (and, sequence)
import Data.Tuple (Tuple(..))
import Data.TypedArray (asUint8Array)
import Data.TypedArray as TA
import Node.Buffer (Buffer)
import Node.Buffer as B
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toNonce, toBoxPubkey, toMessage, encryptOneTimeBallot)
import SecureVote.Crypto.NativeEd25519 (sha256)
import SecureVote.Democs.SwarmMVP.Ballot (makeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (BallotResult, EncBallotWVoter, Erc20Contract(..), SwmVotingContract(..), ballotPropHelper, ballotPropHelperAff, getAccount, getBallotEncPK, makeErc20Contract, makeSwmVotingContract, noArgs, releaseSecretKey, runBallotCount, setBallotEndTime, setWeb3Provider, web3CastBallot)
import SecureVote.Democs.SwarmMVP.KeyGen (generateKey)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex)
import SecureVote.Utils.Numbers (intToStr)
import SecureVote.Utils.Time (currentTimestamp)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)


type AffAll e a = Aff (cp :: CHILD_PROCESS, console :: CONSOLE, now :: NOW, naclRandom :: NACL_RANDOM, random :: RANDOM | e) a


rpcPort :: Number
rpcPort = 8545.0

rpcPortStr :: String
rpcPortStr = toString rpcPort


-- Don't set this to more than 200 (we generate 210 accounts in testrpc during the auto-tests)
-- Note: TestRPC seems to break between the 500 and 700 mark (500 works).
nVotes :: Int
nVotes = 10


logBuffer str = unsafePerformEff $ B.toString UTF8 str

logUC = log <<< unsafeCoerce

self :: forall a. a -> a
self a = a

unsFromJ :: forall a. Maybe a -> a 
unsFromJ a = unsafePartial $ fromJust a


completeBallotTest :: forall e. SpecType (e)
completeBallotTest = do
    it "should compile and deploy the contract, cast randomised votes, retrived and decrypt them, and count those votes correctly." do
        -- setup rpc server
        -- testRpc <- testRpcServer rpcPort
        let _ = setWeb3Provider $ "http://localhost:" <> rpcPortStr
        let voterIds = range 1 nVotes
        
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

        deployErc20Out <- deployArbitrary "Erc20"
        deployErc20Str <- liftEff $ B.toString UTF8 deployErc20Out

        -- get contract object
        let addrM = contractAddrM deployStr
        let contractM = outputToVotingContract addrM
        contract <- maybe (throwError $ error "Unable to get voting contract") pure contractM
        log $ "Contract deployed at: " <> (unsafePartial $ fromJust addrM)

        -- check owner
        votingOwner <- ballotPropHelperAff "owner" [] contract
        coinbase <- either (throwError <<< error) pure (getAccount 0)
        votingOwner `shouldEqual` coinbase

        -- do erc20
        let erc20AddrM = contractAddrM deployErc20Str
        erc20Contract <- maybe (throwError $ error "erc20 contract was not created") pure (erc20AddrM >>= makeErc20Contract)
        log $ "Erc20 deployed at: " <> (unsFromJ erc20AddrM)
        erc20Owner <- ballotPropHelperAff "owner" [] erc20Contract
        erc20Owner `shouldEqual` coinbase

        -- give all voters some coins
        balances <- genBalances voterIds
        setBalanceTxs <- parTraverse (\{addr, newBal} -> ballotPropHelperAff "transfer" [addr, intToStr newBal] erc20Contract) balances
        balancesMatch <- checkBalances erc20Contract balances
        balancesMatch `shouldEqual` true
        
        -- create lots of ballots
        ballots <- createBallots contractM
        log $ "5 sample ballots: " <> (joinWith ", " $ map toHex $ Array.take 5 ballots)

        let encdBallots = encryptBallots (ui8PK) ballots
        log $ "Encrypted " <> (toStringAs decimal $ length encdBallots) <> " ballots."
        nVotes `shouldEqual` (length encdBallots)

        -- publish ballots
        let enumeratedBallots = zip voterIds encdBallots
        log "Casting ballots"
        ballotTxids <- castBallots enumeratedBallots contractM
        logUC $ head ballotTxids

        -- end the ballot
        setBallotTxid <- setBallotEndTime 1508822279 contract  -- corresponds to 2017/10/24 5:17:58 UTC
        logUC setBallotTxid

        -- release secret key
        releaseSKTxid <- releaseSecretKey encSk contract
        logUC releaseSKTxid

        -- -- count ballot
        ballotResultE <- runBallotCount contract erc20Contract

        -- -- check count results
        ballotSuccess <- logAndPrintResults ballotResultE
        ballotSuccess `shouldEqual` true 

        let (nVotesInContract :: Int) = unsFromJ $ (fromStringAs decimal) $ unsafePartial $ fromRight $ ballotPropHelper "nVotesCast" noArgs contract 
        nVotesInContract `shouldEqual` nVotes

        pure unit
  where
    contractAddrM outputStr = Just outputStr >>= extractContractAddr
    outputToVotingContract addrM = addrM >>= makeSwmVotingContract
    getEncPk cM = do
        contract <- maybe (Left "No contract") Right cM
        getBallotEncPK contract
    

compileSol :: forall e. Array String -> AffAll e Buffer
compileSol args = do
        affExec "./bin/solidity/compile.sh" args


deploySol :: forall e. AffAll e {pk :: String, sk :: String, outBuffer :: Buffer}
deploySol = do
        nowTime <- liftEff currentTimestamp
        let endTime = toString $ nowTime + 60.0
        {sk, pk} <- liftEff generateKey
        outBuffer <- affExec "node" ["./bin/solidity/deploy.js", "--startTime", "0", 
                "--endTime", endTime, "--ballotEncPubkey", "0x" <> pk,
                "--unsafeSkipChecks", "--deploy", "--web3Provider",
                "http://localhost:" <> rpcPortStr, "--testing"]
        log $ "Generated Encryption PublicKey : " <> pk
        log $ "Generated Encryption SecretKey : " <> sk
        pure $ {pk, sk, outBuffer}


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


type Balance = {addr :: String, newBal :: Int}
genBalances :: forall e. Array Int -> AffAll e (Array Balance)
genBalances voterIds = do
        sequence $ map genBal voterIds
    where
        genBal voterId = do
            newBal <- liftEff $ randomInt 10 1000
            addr <- either (throwError <<< error) pure (getAccount voterId)
            pure $ {addr, newBal}


checkBalances :: forall e. Erc20Contract -> Array Balance -> AffAll e Boolean
checkBalances erc20 balances = do
        balanceChecks <- parTraverse checkBal balances
        pure $ and balanceChecks
    where
        checkBal {addr, newBal} = do
            bal <- ballotPropHelperAff "balanceOf" [addr] erc20
            pure $ fromString bal == Just newBal


createBallots :: forall e. Maybe SwmVotingContract -> AffAll e ((Array Uint8Array))
createBallots cM = case cM of
    Nothing -> throwError $ error "Contract is Nothing!"
    Just contract -> do
        let encPkM = getBallotEncPK contract
        case encPkM of 
            Left _ -> pure []
            Right encPk -> do
                liftEff $ genBallots nVotes


genBallots :: forall e. Int -> Eff (random :: RANDOM | e) (Array Uint8Array)
genBallots 0 = pure []
genBallots n = do
    setDelegate <- randomInt 0 1
    delegateE <- if setDelegate == 1 then getAccount <$> randomInt 1 nVotes else pure $ Right "0x1111122222333334444411111222223333344444"
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


logAndPrintResults :: forall e. Either String BallotResult -> Aff (console :: CONSOLE | e) Boolean
logAndPrintResults ballotResults = case ballotResults of 
    Left err -> do
        log $ "Ballot Count Failed!\n" <> err
        pure false
    Right res -> do
        log $ "Ballot count success!"
        logUC res 
        pure true 



affExec :: forall eff. String -> Array String -> Aff (cp :: CHILD_PROCESS | eff) Buffer
affExec cmd args = makeAff $ \cb -> do
    execFile cmd args defaultExecOptions $ \ex -> cb $ maybe (Right ex.stdout) Left ex.error
    pure $ Canceler (\_ -> do
        _ <- throwError $ error $ "Canceled: " <> cmd <> (joinWith " " args)
        pure unit)

