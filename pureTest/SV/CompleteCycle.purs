module Test.SV.CompleteCycle where
  
import Prelude

import Control.Monad.Aff (Aff, Canceler(..), error, makeAff, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (NACL_RANDOM, box, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Array (drop, tail, dropWhile, head, range, replicate, zip, (:))
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..), fromRight)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Number.Format (toString)
import Data.String (joinWith)
import Data.String.CodePoints (take)
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..))
import Data.TypedArray as TA
import Node.Buffer (Buffer)
import Node.Buffer as B
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key)
import SecureVote.Democs.SwarmMVP.Ballot (makeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (SwmVotingContract(..), noArgs, ballotPropHelper, ballotPropHelperAff, getBallotEncPK, makeSwmVotingContract, releaseSecretKey, runBallotCount, setBallotEndTime, setWeb3Provider, web3CastBallot, BallotResult)
import SecureVote.Democs.SwarmMVP.KeyGen (generateKey)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex)
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


-- Don't set this to more than 199 (we generate 200 accounts in testrpc during the auto-tests)
nVotes :: Int
nVotes = 30


logBuffer str = unsafePerformEff $ B.toString UTF8 str

logUC = log <<< unsafeCoerce


completeBallotTest :: forall e. SpecType (e)
completeBallotTest = do
    it "should compile and deploy the contract" do
        -- setup rpc server
        -- testRpc <- testRpcServer rpcPort
        let _ = setWeb3Provider $ "http://localhost:" <> rpcPortStr
        
        -- compile contract
        compileOut <- compileSol
        
        -- deploy it
        Tuple encSk deployOut <- deploySol
        let ui8SK = unsafePartial $ fromJust $ fromHex encSk
        deployStr <- liftEff $ B.toString UTF8 deployOut

        -- get contract object
        let contractM = outputToVotingContract deployStr
        
        -- create lots of ballots
        ballots <- createBallots contractM
        log $ unsafeCoerce $ head $ map toHex ballots
        let encdBallots = encryptBallots (ui8SK) ballots
        log $ unsafeCoerce $ head $ map (\(Tuple enc pk) -> Tuple (toHex enc) (toHex pk) ) encdBallots

        -- publish ballots
        let enumeratedBallots = zip (range 1 9999) encdBallots
        log "Casting ballots"
        ballotTxids <- castBallots enumeratedBallots contractM
        logUC $ head ballotTxids

        -- end the ballot
        let contract = unsafePartial $ fromJust contractM
        setBallotTxid <- setBallotEndTime 0 contract
        logUC setBallotTxid

        -- release secret key
        releaseSKTxid <- releaseSecretKey encSk contract
        logUC releaseSKTxid

        -- count ballot
        let ballotResultE = runBallotCount contractM

        -- check count results
        ballotSuccess <- logAndPrintResults ballotResultE
        ballotSuccess `shouldEqual` true 

        let (nVotesInContract :: Int) = ballotPropHelper "nVotesCast" noArgs contract 
        nVotesInContract `shouldEqual` nVotes

        pure unit 
    it "does something else" $ pure unit
  where
    outputToVotingContract outputStr = Just outputStr >>= extractContractAddr >>= makeSwmVotingContract
    getEncPk cM = do
        contract <- maybe (Left "No contract") Right cM
        getBallotEncPK contract
    

compileSol :: forall e. AffAll e Buffer
compileSol = do
        affExec "yarn" ["sol-compile"]


deploySol :: forall e. AffAll e (Tuple String Buffer)
deploySol = do
        nowTime <- liftEff currentTimestamp
        let endTime = toString $ nowTime + 60.0
        {sk, pk} <- liftEff generateKey
        outBuffer <- affExec "yarn" ["sol-deploy", "--startTime", "0", 
                "--endTime", endTime, "--ballotEncPubkey", "0x" <> pk,
                "--unsafeSkipChecks", "--deploy", "--web3Provider",
                "http://localhost:" <> rpcPortStr, "--testing"]
        pure $ Tuple sk outBuffer 


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
    ballot <- makeBallot
    ballots <- genBallots (n-1)
    pure $ ballot : ballots


encryptBallots :: forall e. Uint8Array -> Array Uint8Array -> (Array (Tuple Uint8Array Uint8Array))
encryptBallots _ [] = []
encryptBallots encPk ballots = do
        ballot <- ballots
        let keypair = unsafePerformEff $ genCurve25519Key
        let publicKey = getBoxPublicKey keypair
        let secretKey = getBoxSecretKey keypair
        let nonce = genNonce publicKey
        let encBallot = toUint8Array $ box (unsafeCoerce ballot) (unsafeCoerce nonce) (unsafeCoerce encPk) secretKey
        pure $ (Tuple encBallot (unsafeCoerce publicKey))
    where
      genNonce pk = asUint8Array $ whole $ fromArray $ replicate 24 0.0


castBallots :: forall e. Array (Tuple Int (Tuple Uint8Array Uint8Array)) -> Maybe SwmVotingContract -> Aff (| e) (Array String)
castBallots _ Nothing = pure []
castBallots [] _ = pure []
castBallots allBallots (Just contract) = do
    let (Tuple i (Tuple encBallot senderPk)) = unsafePartial $ fromJust $ head allBallots
    let ballots = unsafePartial $ fromJust $ tail allBallots
    ballotTxid <- web3CastBallot i (Tuple encBallot senderPk) contract 
    ballotTxids <- castBallots ballots (Just contract)
    pure $ ballotTxid : ballotTxids
castBallots _ _ = pure []


logAndPrintResults :: forall e. BallotResult -> Aff (console :: CONSOLE | e) Boolean
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


extractContractAddr :: String -> Maybe String
extractContractAddr output = addr
    where
      lineM = head $ dropWhile (not <<< lineMatches) $ lines output
      lineMatches str = "Contract Addr: 0x" == take 17 str
      addr = lineM >>= (\str' -> head $ drop 2 $ words str')
