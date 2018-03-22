module SV.Light.AuditBallot where

import SV.Light.Types.Ballot
import SV.Light.Types.BallotBox
import SV.Light.Types.RunBallot
import SV.Prelude
import SV.Types.Lenses

import Control.Monad.Aff (ParAff, catchError, error, forkAff, joinFiber, message, parallel, sequential, throwError)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, lift)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxSecretKey)
import Data.Array (concat, foldr, last, range)
import Data.Array as Arr
import Data.Decimal as Dec
import Data.Int (round, toNumber)
import Data.Int as DInt
import Data.Lens ((.~), (^.))
import Data.Map (Map, fromFoldable)
import Data.Map as Map
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Traversable (sequence)
import Global.Unsafe (unsafeStringify)
import IPFS (IPFSEff)
import Network.Ethereum.Web3 (type (:&), Address, BigNumber, Block(..), CallError, ChainCursor(..), D2, D5, D6, ETH, TransactionOptions, UIntN, _to, defaultTransactionOptions, embed, mkAddress, mkHexString, pow, uIntNFromBigNumber, unUIntN, unsafeToInt)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getBlockByNumber)
import Network.Ethereum.Web3.Solidity (Tuple3(..))
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import Network.Ethereum.Web3.Types (Web3)
import Network.Ethereum.Web3.Types.Types (HexString)
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import SV.Light.Delegation (getDelegates)
import SV.Light.IPFS (getBlock)
import SV.Types.OutboundLogs (mkSUFail, mkSULog, mkSUWarn)
import SecureVote.Contracts.Erc20 (balanceOf)
import SecureVote.Contracts.SVLightBallotBox (associatedPubkeys, ballotEncryptionSeckey, ballotMap, creationBlock, endTime, getEncSeckey, nVotesCast, specHash, startTime, startingBlockAround)
import SecureVote.Utils.Array (chunk, fromList, onlyJust)
import SecureVote.Utils.IPFS (hexHashToSha256Bs58)
import SecureVote.Utils.Time (currentTimestamp)
import SecureVote.Utils.Web3Bin (bytesNToHex)
import SecureVote.Web3.Web3 (runWeb3_, zeroHash)
import Simple.JSON (readJSON')


getBallotInfo :: forall e. {bScAddr :: Address} -> Aff (eth :: ETH, ref :: REF | e) BallotInfo
getBallotInfo {bScAddr} = do
    sequential $
        mkBallotInfo <$> (map bytesNToHex $ pw3 $ specHash tos Latest)
                     <*> (map uintToInt $ pw3 $ startTime tos Latest)
                     <*> (map uintToInt $ pw3 $ endTime tos Latest)
                     <*> (map (skCheck <<< bytesNToHex) $ pw3 $ ballotEncryptionSeckey tos Latest)
                     <*> (map uintToInt $ pw3 $ nVotesCast tos Latest)
                     <*> (map uintToInt $ pw3 $ creationBlock tos Latest)
                     <*> (map uintToInt $ pw3 $ startingBlockAround tos Latest)
  where
    pw3 :: forall e2 a. Web3 (ref :: REF | e2) (Either CallError a) -> ParAff (eth :: ETH, ref :: REF | e2) a
    pw3 = parallel <<< (eToAff <=< eToAff <=< runWeb3_)
    -- transaction options
    tos = defaultTransactionOptions # _to .~ Just bScAddr
    uintToInt :: forall m a n. KnownSize n => UIntN n -> Int
    uintToInt = unsafeToInt <<< unUIntN
    skCheck a = if a == zeroHash then Nothing else Just a


getBallotSpec :: forall e. HexString -> Aff (ref :: REF, ipfs :: IPFSEff, buffer :: BUFFER | e) BallotSpec
getBallotSpec h = do
    block <- getBlock (hexHashToSha256Bs58 h)
    (exceptToAff <<< readJSON') =<< (liftEff $ Buffer.toString UTF8 block."data")


runBallotCount :: RunBallotArgs -> (_ -> Unit) -> ExceptT String (Aff _) BallotResult
runBallotCount {bInfo, bSpec, bbTos, ercTos, dlgtTos, silent} updateF = do
    nowTime <- lift $ liftEff $ round <$> currentTimestamp
    let endTime = bSpec ^. _endTime
        startTime = bInfo.startTime
        tknAddr = unsafePartial fromJust $ ercTos ^. _to
    log $ "Ballot StartTime: " <> show startTime <> ", Ballot EndTime: " <> show endTime <> ", Current Time: " <> show nowTime

    if bSpec ^. _options == OptsBinary
        then pure unit
        else do
            fail "Only binary ballots supported currently"
            throwError "Only binary ballots supported currently"

    startingBlockFibre <- lift $ forkAff $ do
        logAff $ "Finding Eth block close to time: " <> show startTime <> " (takes 10-20 seconds)"
        startBlock <- findEthBlockEndingInZeroBefore startTime
        logAff $ "Using block " <> show startBlock <> " for ERC20 balances."
        pure startBlock

    -- check that we can proceed
    let encPkM = bSpec ^. _encryptionPK
    secKey <- bytesNToHex <$> w3BB getEncSeckey Latest
    case (Tuple (nowTime < endTime) (isNothing encPkM || secKey /= zeroHash)) of
        Tuple false true -> warn "Ballot has not ended, determining live results..."
        Tuple true false -> throwError "Error: The ballot has ended but I cannot determine the results due as the secret key has not been released"
        _ -> pure unit

    if secKey /= zeroHash then log $ "The ballot encryption secret key is " <> show zeroHash else log "Ballot is not encrypted, proceeding..."

    nVotes <- (unsafeToInt <<< unUIntN) <$> w3BB nVotesCast Latest
    log $ "Ballot smart contract reports " <> show nVotes <> " were cast."

    log "Retrieving votes now. This may take some time."
    ballotProgress <- lift $ makeVar 0
    rawBallotsWDupes <- lift $ getBallots bbSC nVotes (incrementBallotProgress nVotes logAff ballotProgress)
    log $ "Retrieved " <> (show $ Arr.length rawBallotsWDupes) <> " votes"

    rawBallotsNoDupes <- removeDupes rawBallotsWDupes
    log $ "Removing repeated votes took nVotes from " <>
            (show $ Arr.length rawBallotsWDupes) <> " to " <>
            (show $ Arr.length rawBallotsNoDupes)

    plaintextBallots <- if isJust (bSpec ^. _encryptionPK) then do
            -- decryptedBallots <- lift $ decryptBallots secKey rawBallotsNoDupes logAff
            -- log $ "Decrypted " <> (show $ Arr.length decryptedBallots) <> " votes successfully"
            -- pure decryptedBallots
            throwError "Ballot decryption not yet supported"
        else pure rawBallotsNoDupes

    let ballotMap = Map.fromFoldable $ (\b@{voterAddr} -> Tuple voterAddr b) <$> plaintextBallots

    ballotStartBlock <- lift $ joinFiber startingBlockFibre
    let ballotStartCC = BN $ wrap $ embed ballotStartBlock

    delegateMap <- lift $ getDelegates {tknAddr, allBallots: plaintextBallots} dlgtSC Latest
    log $ "Found " <> show (Map.size delegateMap) <> " relevant delegations"

    let allVoters = (\{voterAddr} -> voterAddr) <$> plaintextBallots
    -- also get all ppl who have made some delegation that relates to some vote
    let allRelevantTknHolders = fromList $ Map.keys delegateMap
    balanceMap <- lift $ removeBannedAddrs <$> getBalances ercSC ballotStartBlock (allVoters <> allRelevantTknHolders)
    log $ "Got " <> show (Map.size balanceMap) <> " total balances"

    -- | loop through addrs in balance map to find the first associated vote and associate balances
    let (weightedBallots :: Array _) = onlyJust $ getVoteOrRecurse ballotMap delegateMap <$> Map.toUnfoldable balanceMap

    log $ "Calculating final results..."
    let results = foldr (\(Tuple ({ballot}) bal) m ->
                                Map.lookup ballot m
                                # fromMaybe (embed 0)
                                # \v -> Map.insert ballot (v + bal) m)
                        Map.empty weightedBallots

    let ballotYes = unsafePartial fromJust $ mkHexString "8000000000000000000000000000000000000000000000000000000000000000"
        ballotNo = unsafePartial fromJust $ mkHexString "4000000000000000000000000000000000000000000000000000000000000000"

    let getRes a = procBal $ Map.lookup a results
    pure {nVotes, ballotResults: {yes: getRes ballotYes, no: getRes ballotNo}}
  where
    w3BB :: forall a b. (TransactionOptions _ -> b -> Web3 _ (Either CallError a)) -> b -> ExceptT String (Aff _) a
    w3BB r = w3Gen r bbTos
    w3Erc r = w3Gen r ercTos
    w3Gen :: forall a b. (TransactionOptions _ -> b -> Web3 _ (Either CallError a)) -> TransactionOptions _ -> b -> ExceptT String (Aff _) a
    w3Gen r tos = convE <<< lift <<< (eToAff <=< runWeb3_) <<< r tos

    bbSC :: forall args e a. SmartContract e args a
    bbSC = genericSC bbTos
    -- bbSC f c args = eToAff <=< eToAff <=< runWeb3_ $ f bbTos c args

    ercSC :: forall args e a. SmartContract e args a
    ercSC = genericSC ercTos
    -- ercSC f c args = eToAff <=< eToAff <=< runWeb3_ $ f ercTos c args

    dlgtSC :: forall args e a. SmartContract e args a
    dlgtSC = genericSC dlgtTos

    genericSC :: forall args e a. TxOpts -> SmartContract e args a
    genericSC tos f c args = eToAff <=< eToAff <=< runWeb3_ $ f tos c args

    log :: forall e. String -> ExceptT String (Aff _) Unit
    log str = lift $ logAff str

    logAff :: forall e. String -> Aff _ Unit
    logAff str = do
        let _ = updateF $ mkSULog str
        if silent then pure unit else AffC.log str

    warnAff str = do
        let _ = updateF $ mkSUWarn str
        if silent then pure unit else AffC.warn str

    failAff str = do
        let _ = updateF $ mkSUFail str
        if silent then pure unit else AffC.error str


    warn :: String -> ExceptT String (Aff _) Unit
    warn = lift <<< warnAff
    fail :: String -> ExceptT String (Aff _) Unit
    fail = lift <<< failAff

    convE :: forall m a. MonadThrow String m => m (Either CallError a) -> m a
    convE me = either (throwError <<< (<>) "Web3 Call Error: " <<< show) pure =<< me

    procBal mtotal = (fromMaybe (embed 0) mtotal) * (recip $ pow (embed 10) 18)


-- | Log and increment the number of ballots we've processed to facilitate progress updates
incrementBallotProgress :: forall e. Int -> (String -> Aff (avar :: AVAR | e) Unit) -> AVar Int -> Aff (avar :: AVAR | e) Unit
incrementBallotProgress totalBallots log avar = do
    n <- (+) 1 <$> takeVar avar
    putVar n avar
    if n `mod` 10 == 0
        then log $ "Processed " <> show n <> " ballots; " <> show (n * 100 / totalBallots) <> "% done."
        else pure unit


-- | Get ballots from SC
getBallots :: forall e m. (forall args a. SmartContract _ args a) -> Int -> Aff _ Unit -> Aff _ (Array BallotFromSC)
getBallots bbSC n incBallotProgress
    | n <= 0 = pure []
    | otherwise = do
        let allVoteIds = range 0 (n-1)
        let (chunks :: Array (Array Int)) = chunk 25 allVoteIds
        let (toPar :: Array (Aff _ (Array BallotFromSC))) = (parTraverse getBallot) <$> chunks
        map concat $ sequence $ toPar
    where
        getBallot i = do
            ballotN <- mToAff ("Unable to convert " <> show i <> " to uint256!") $ uIntNFromBigNumber $ embed i
            (Tuple3 ballotBytesN voterAddr blockNumberUint) <- bbSC ballotMap Latest ballotN
            voterPk <- bytesNToHex <$> bbSC associatedPubkeys Latest ballotN
            let ballot = bytesNToHex ballotBytesN
            incBallotProgress
            pure $ {i, ballot, voterPk, voterAddr}


-- todo, this is going to be slow -- might be much faster to reverse the array...
-- implemented as per 4.9.3 in Swarm Voting Spec
removeDupes :: forall e m. MonadThrow String (m (Aff e)) => Array BallotFromSC -> m (Aff e) (Array BallotFromSC)
removeDupes [] = pure []
removeDupes ballots = do
        lastBallot <- maybe (throwError "ballots did not have a `last` element - but we checked for that!") pure $ last ballots
        let remBallots = Arr.filter (\b -> b.voterAddr /= lastBallot.voterAddr) ballots
        (<>) <$> (removeDupes remBallots) <*> pure [lastBallot]


-- -- | Decrypt those ballots
-- decryptBallots :: forall e e2. BoxSecretKey -> Array BallotFromSC -> (String -> Aff (console :: CONSOLE | e) Unit) -> Aff (console :: CONSOLE | e) (Array BallotFromSC)
-- decryptBallots _ [] log = pure []
-- decryptBallots encSk ballots log = do
--         ballots_ <- parTraverse decryptOne ballots
--         let cleanBallots = map (unsafePartial $ fromRight) (filter isRight ballots_)
--         let badDecryptions = map (unsafePartial $ fromLeft) (filter isLeft ballots_)
--         if length badDecryptions > 0 then
--                 log $ "Got " <> (show $ length badDecryptions) <> " bad decryptions: " <> show badDecryptions
--             else
--                 log "All ballots decrypted successfully."
--         pure cleanBallots
--     where
--         decryptOne {i, encBallot, voterPk, voterAddr} = do
--             let ballotM = toUint8Array <$> (decryptOneTimeBallot (toBox encBallot) (toBoxPubkey voterPk) encSk)
--             maybe (pure $ Left $ "Cannot decrypt ballot from: " <> show voterAddr) (\ballot -> pure $ Right {ballot, voterPk, voterAddr}) ballotM


-- | Get balances
getBalances :: forall m e. SmartContract e {_owner :: Address} (UIntN (D2 :& D5 :& D6)) -> Int -> Array Address -> Aff (eth :: ETH, ref :: REF | e) BalanceMap
getBalances w3Erc blockNumber ballots = do
        pairs <- parTraverse addBalance ballots
        pure $ fromFoldable pairs
    where
        addBalance voterAddr = do
            balance <- unUIntN <$> w3Erc balanceOf (BN $ wrap $ embed blockNumber) {_owner: voterAddr}
            pure $ Tuple voterAddr balance

bannedAddrs :: Set.Set Address
bannedAddrs = Set.fromFoldable $ (unsafePartial fromJust <<< (mkAddress <=< mkHexString)) <$>
    [ "0x8bf7b2d536d286b9c5ad9d99f608e9e214de63f0" -- SWM Foundation
    ]

removeBannedAddrs :: Map Address _ -> Map Address _
removeBannedAddrs = Map.filterKeys (\k -> not $ Set.member k bannedAddrs)


findEthBlockEndingInZeroBefore :: Int -> Aff _ Int
findEthBlockEndingInZeroBefore targetTime = do
    let initLowBlock = 0
    currBlock <- runWeb3_ eth_blockNumber >>= eToAff <#> (unwrap >>> unsafeToInt)
    Tuple currBlockTs lowTs <- sequential $ Tuple
        <$> parallel (getBlockTimestamp currBlock)
        <*> parallel (getBlockTimestamp initLowBlock)

    -- take a guess first to try and get close
    let ts1Scaled = (toNumber $ (targetTime - lowTs) / 100000)
        ts2Scaled = (toNumber $ (currBlockTs - lowTs) / 100000)
        bScaled = (toNumber $ (currBlock - initLowBlock) / 1000)
    let midStep = (ts1Scaled * bScaled) / ts2Scaled
    let finalStepF = add initLowBlock <<< mul 1000 <<< DInt.round
    let gBH = min (finalStepF $ midStep * 1.1) currBlock
    let gBL = max (finalStepF $ midStep * 0.9) initLowBlock
    Tuple gHTs gLTs <- sequential $ Tuple <$> parallel (getBlockTimestamp gBH) <*> parallel (getBlockTimestamp gBL)

    AffC.log $ "Searching for block with targetTs: " <> show targetTime <> ", currBlockTs: " <> show currBlockTs <> ", currBlock: " <> show currBlock
    let runF = _findLastEthBlockBefore targetTime
    if currBlockTs < targetTime || targetTime < lowTs
        then throwError $ error $ "Cannot find Eth block at " <> show targetTime <> " because it is outside range: " <> show lowTs <> ", " <> show currBlockTs
        else case Tuple (compare gHTs targetTime) (compare gLTs targetTime) of
            -- if upper guess is LT target time
            Tuple LT _ -> runF {hTs: currBlockTs, hB: currBlock, lTs: gHTs, lB: gBH}
            -- if lower guess is GT target time
            Tuple _ GT -> runF {hTs: gLTs, hB: gBL, lTs: lowTs, lB: initLowBlock}
            -- if we hit the money in any way
            Tuple EQ _ -> pure gBH
            Tuple _ EQ -> pure gBL
            -- otherwise we're in between
            Tuple GT LT -> runF {hTs: gHTs, hB: gBH, lTs: gLTs, lB: gBL}
  where
    _findLastEthBlockBefore :: Int -> {hTs :: Int, hB :: Int, lTs :: Int, lB :: Int} -> Aff _ Int
    _findLastEthBlockBefore tTime {hTs, hB, lTs, lB} = do
        case compare hB lB of
                LT -> go tTime lTs lB hTs hB
                EQ -> pure lB
                GT -> go tTime hTs hB lTs lB
      where
        go tTime hTs hB lTs lB = do
            AffC.log $ "Block search: blockN diff: " <> show (hB - lB) <> ", Target: " <> show tTime

            let testBlockN = (hB - lB) / 2 + lB
            newTs <- getBlockTimestamp testBlockN

            case compare newTs tTime of
                GT -> _findLastEthBlockBefore tTime {hTs: newTs, hB: testBlockN, lTs, lB}
                EQ -> pure $ testBlockN
                LT -> if hB - lB == 1 then pure $ lB else _findLastEthBlockBefore tTime {hTs, hB, lTs: newTs, lB: testBlockN}
    getBlockTimestamp blkNum = runWeb3_ (eth_getBlockByNumber (BN $ wrap $ embed blkNum)) >>= eToAff <#> (\(Block b) -> b.timestamp # unsafeToInt)


-- | This takes a ballotMap, delegateMap, and a (voter, balance) - it'll find the _first_ ballot in the
-- | delegation chain and associate the balance with that ballot.
getVoteOrRecurse :: BallotMap -> DelegateMap -> Tuple Address BigNumber -> Maybe (Tuple BallotFromSC BigNumber)
getVoteOrRecurse ballotMap delegateMap p@(Tuple voter balance) = do
    let _ = unsafePerformEff $ EffC.log $ "Returning for voter " <> show voter <> " balance " <> show balance <> " with ballot " <> unsafeStringify res
    res
  where
    res = case Map.lookup voter ballotMap of
        Just ballot -> Just $ Tuple ballot balance
        Nothing -> case Map.lookup voter delegateMap of
            Just dlgt -> getVoteOrRecurse ballotMap delegateMap p
            Nothing -> Nothing
