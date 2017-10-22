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
import Data.Array (drop, dropWhile, head, replicate, (:))
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String (joinWith)
import Data.String.CodePoints (take)
import Data.String.Utils (lines, words)
import Node.Buffer (Buffer)
import Node.Buffer as B
import Node.ChildProcess (CHILD_PROCESS, defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import SecureVote.Crypto.Curve25519 (genCurve25519Key)
import SecureVote.Democs.SwarmMVP.Ballot (makeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (SwmVotingContract(..), getBallotEncPK, makeSwmVotingContract, setWeb3Provider)
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


logBuffer str = unsafePerformEff $ B.toString UTF8 str


completeBallotTest :: forall e. SpecType (e)
completeBallotTest = do
    it "should compile and deploy the contract" do
        -- setup rpc server
        -- testRpc <- testRpcServer rpcPort
        let _ = setWeb3Provider $ "http://localhost:" <> rpcPortStr
        
        -- compile contract
        compileOut <- compileSol
        
        -- deploy it
        deployOut <- deploySol
        deployStr <- liftEff $ B.toString UTF8 deployOut

        -- get contract object
        let contractM = outputToVotingContract deployStr
        -- let encPk = 
        
        -- create lots of ballots
        ballots <- createBallots contractM
        log $ unsafeCoerce $ map toHex ballots
        let encdBallots = encryptBallots ballots
        log $ unsafeCoerce $ map toHex encdBallots

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


deploySol :: forall e. AffAll e Buffer
deploySol = do
        nowTime <- liftEff currentTimestamp
        let endTime = toString $ nowTime + 60.0
        {sk, pk} <- liftEff generateKey
        affExec "yarn" ["sol-deploy", "--startTime", "0", 
                "--endTime", endTime, "--ballotEncPubkey", "0x" <> pk,
                "--unsafeSkipChecks", "--deploy", "--web3Provider",
                "http://localhost:" <> rpcPortStr]


createBallots :: forall e. Maybe SwmVotingContract -> AffAll e ((Array Uint8Array))
createBallots cM = case cM of
    Nothing -> throwError $ error "Contract is Nothing!"
    Just contract -> do
        let encPkM = getBallotEncPK contract
        case encPkM of 
            Left _ -> pure []
            Right encPk -> do
                liftEff $ genBallots 100


genBallots :: forall e. Int -> Eff (random :: RANDOM | e) (Array Uint8Array)
genBallots 0 = pure []
genBallots n = do
    ballot <- makeBallot
    ballots <- genBallots (n-1)
    pure $ ballot : ballots


encryptBallots :: forall e. Array Uint8Array -> Array Uint8Array
encryptBallots [] = []
encryptBallots ballots = do
        ballot <- ballots
        let keypair = unsafePerformEff $ genCurve25519Key
        let publicKey = getBoxPublicKey keypair
        let secretKey = getBoxSecretKey keypair
        let nonce = genNonce publicKey
        pure $ toUint8Array $ box (unsafeCoerce ballot) (unsafeCoerce nonce) (publicKey) secretKey
    where
      genNonce pk = asUint8Array $ whole $ fromArray $ replicate 24 0.0


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
