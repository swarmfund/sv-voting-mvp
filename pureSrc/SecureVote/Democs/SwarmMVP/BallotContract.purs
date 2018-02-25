module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff (Aff, catchError, error, forkAff, joinFiber, launchAff, liftEff', message, parallel, sequential, throwError)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, readVar, takeVar)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxPublicKey, BoxSecretKey, toUint8Array)
import Data.Array (concat, elem, filter, foldl, head, init, last, length, range, replicate, sortBy, tail, takeWhile, zip, (:))
import Data.Array as A
import Data.ArrayBuffer.Typed (toIntArray)
import Data.ArrayBuffer.Types (ArrayView, Uint8, Uint8Array)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Decimal (Decimal)
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromLeft, fromRight, isLeft, isRight)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5, runFn6)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, decimal, fromStringAs, toNumber, toStringAs)
import Data.Int as DInt
import Data.Map (Map, delete, empty, fromFoldable, insert, keys, lookup, showTree, size, values, toUnfoldable)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (class Newtype)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (Pattern(..), drop, joinWith)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (maximumBy, sequence, sum, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Math (round, (%))
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import SecureVote.Crypto.Curve25519 (decryptOneTimeBallot, toBox, toBoxPubkey, toBoxSeckey)
import SecureVote.Democs.SwarmMVP.Admin (main)
import SecureVote.Democs.SwarmMVP.Ballot (delegateAddr)
import SecureVote.Democs.SwarmMVP.BannedAddrs (bannedAddresses)
import SecureVote.Democs.SwarmMVP.Types (Address, Delegate, SwmBallot, Vote, Voter, Votes, VotesRecord, getVoter, getVotes, toAddress, toDelegate, toSwmBallot, toVoter, voteToInt, voterToString)
import SecureVote.Utils.Array (fromList, chunk)
import SecureVote.Utils.ArrayBuffer (UI8AShowable(..), fromEthHex, fromEthHexE, fromHex, fromHexE, toHex)
import SecureVote.Utils.ArrayBuffer as SVAB
import SecureVote.Utils.Monads (mToE)
import SecureVote.Utils.Numbers (intByteToBitStr, intToStr)
import SecureVote.Utils.Poly (self)
import SecureVote.Utils.String (padLeft)
import SecureVote.Utils.Time (currentTimestamp)
import Unsafe.Coerce (unsafeCoerce)


class Web3Contract a


-- These types can never be instantiated from purescript, they can be given from the FFI though
data SwmVotingContract = SwmVotingContract SwmVotingContract
instance swmWeb3 :: Web3Contract SwmVotingContract

data Erc20Contract = Erc20Contract Erc20Contract
instance erc20Web3 :: Web3Contract Erc20Contract


type BallotResult = {winner :: Maybe (Tuple String Decimal), possibleWinners :: Array (Tuple String Decimal), totals :: Array (Tuple String Decimal)}


type Ballot = {ballot :: Uint8Array, voterPk :: Uint8Array, voterAddr :: Voter}
type EncBallot = {i :: Int, encBallot :: Uint8Array, voterPk :: Uint8Array, voterAddr :: Voter}
type EncBallotWVoter = {encBallot :: Uint8Array, voterPk :: BoxPublicKey}


type DelegateMap = Map Voter (Maybe Voter)
type BallotMap = Map Voter (Either String SwmBallot)
type BalanceMap = Map Voter Decimal


type BalanceBallotPair = {balance :: Decimal, ballot :: VotesRecord}
type ScaledVotes = {v1s :: Array Decimal, v2s :: Array Decimal, v3s :: Array Decimal, v4s :: Array Decimal, v5s :: Array Decimal}


newtype OptStrs = OptStrs {o1 :: String, o2 :: String, o3 :: String, o4 :: String, o5 :: String}
instance optStrShow :: Show OptStrs where
    show (OptStrs {o1, o2, o3, o4, o5}) = String.joinWith ", " [o1,o2,o3,o4,o5]


data SUAux = SuStr String | SuRes AllDetails


type StatusUpdate =
    { t :: String
    , p :: SUAux
    }


mkSUFail :: String -> StatusUpdate
mkSUFail e = {t: "fail", p: SuStr e}


mkSULog :: String -> StatusUpdate
mkSULog p = {t: "log", p: SuStr p}


mkSUSuccess :: AllDetails -> StatusUpdate
mkSUSuccess b = {t: "success", p: SuRes b}


type AllDetails =
    { ballotSeckey :: BoxSecretKey
    , nVotes :: Int
    , encBallotsWithDupes :: Array EncBallot
    , encBallotsWithoutDupes :: Array EncBallot
    , decryptedBallots :: Array Ballot
    , delegateMap :: DelegateMap
    , delegateMapNoHang :: DelegateMap
    , delegateMapNoLoops :: DelegateMap
    , ballotMap :: BallotMap
    , ballotResult :: BallotResult
    , balanceMap :: BalanceMap
    }


noArgs :: forall a. Array a
noArgs = []


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import makeErc20ContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe Erc20Contract)
foreign import setWeb3ProviderImpl :: forall e a. Fn2 String String (EffFnAff (| e) Unit)

-- web3 FFI
foreign import getAccountImpl :: forall a b. Fn3 (a -> Either a b) (b -> Either a b) Int (Either String String)
foreign import getBlockNumberImpl :: forall e. Fn0 (EffFnAff (| e) Int)
foreign import getBlockTimstampImpl :: forall e. Fn1 Int (EffFnAff (| e) Int)

getBlockNumber :: forall eff. Aff (| eff) Int
getBlockNumber = fromEffFnAff (runFn0 getBlockNumberImpl)

getBlockTimestamp :: forall e. Int -> Aff (| e) Int
getBlockTimestamp blockNum = fromEffFnAff (runFn1 getBlockTimstampImpl blockNum)

-- contract FFI
foreign import getBallotSKImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) SwmVotingContract (Maybe a)
foreign import getBallotPropImpl :: forall a b c w. Fn5 (a -> Either a b) (b -> Either a b) String c w (Either a b)

-- TODO: These functions which take an `Array a` require all elements to the the same type, but that's often not what eth parameters are...
-- workaround is to submit ints as strings, this works with the web3 api currently
-- however, submitting bools, arrays, etc are beyond this atm
ballotPropHelper :: forall a b c w. (Web3Contract w) => String -> Array a -> w -> Either b c
ballotPropHelper = runFn5 getBallotPropImpl Left Right

foreign import getBallotPropAsyncImpl :: forall e b c w. Fn3 String (Array c) w (EffFnAff (| e) b)
foreign import getBallotPropAsyncWBlockNumImpl :: forall e b c w. Fn4 String (Array c) Int w (EffFnAff (| e) b)

ballotPropHelperEffFnAff :: forall a b eff w. (Web3Contract w) => String -> Array a -> w -> EffFnAff (| eff) b
ballotPropHelperEffFnAff = runFn3 getBallotPropAsyncImpl

ballotPropHelperWBlockNumEffFnAff :: forall a b eff w. (Web3Contract w) => String -> Array a -> Int -> w -> EffFnAff (| eff) b
ballotPropHelperWBlockNumEffFnAff = runFn4 getBallotPropAsyncWBlockNumImpl

ballotPropHelperAff :: forall a eff w. (Web3Contract w) => String -> (Array a) -> w -> (Aff (| eff) String)
ballotPropHelperAff prop args contract = fromEffFnAff (runFn3 getBallotPropAsyncImpl prop args contract)

ballotPropHelperWBlockNumAff :: forall a eff w. (Web3Contract w) => String -> (Array a) -> Int -> w -> (Aff (| eff) String)
ballotPropHelperWBlockNumAff prop args extras contract = fromEffFnAff (runFn4 getBallotPropAsyncWBlockNumImpl prop args extras contract)

foreign import submitBallotImpl :: forall e. Fn4 Int Uint8Array BoxPublicKey SwmVotingContract (EffFnAff (| e) String)


-- contract setup functions
makeVotingContract :: forall a. String -> Maybe SwmVotingContract
makeVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

makeErc20Contract :: forall a. String -> Maybe Erc20Contract
makeErc20Contract = runFn3 makeErc20ContractImpl Just Nothing


setWeb3Provider :: forall a e. String -> String -> Aff (| e) Unit
setWeb3Provider host auth = fromEffFnAff (runFn2 setWeb3ProviderImpl host auth)

-- web3 functions
getAccount :: Int -> Either String String
getAccount = runFn3 getAccountImpl Left Right

-- contract functions
swmBallotSk :: forall e. SwmVotingContract -> Aff (| e) (BoxSecretKey)
swmBallotSk contract = do
    skStr <- ballotPropHelperAff "ballotEncryptionSeckey" noArgs contract
    if skStr == "0x0000000000000000000000000000000000000000000000000000000000000000" then
            throwError $ error "Ballot Encryption Secret Key has not yet been published"
        else either (throwError <<< error) (pure <<< toBoxSeckey) (fromEthHexE skStr)


getBallotEndTime :: forall a. SwmVotingContract -> Either String String
getBallotEndTime = ballotPropHelper "endTime" noArgs


getBallotEncPK :: SwmVotingContract -> Either String String
getBallotEncPK = ballotPropHelper "ballotEncryptionPubkey" noArgs


setBallotEndTime :: forall e. Int -> SwmVotingContract -> Aff (| e) String
setBallotEndTime endTime contract = fromEffFnAff $ ballotPropHelperEffFnAff "setEndTime" [endTime] contract


releaseSecretKey :: forall e. String -> SwmVotingContract -> Aff (| e) String
releaseSecretKey secKey contract = fromEffFnAff $ ballotPropHelperEffFnAff "revealSeckey" ["0x" <> secKey] contract


web3CastBallot :: forall e. Int -> EncBallotWVoter -> SwmVotingContract -> (Aff (| e) (String))
web3CastBallot accN {encBallot, voterPk} contract = fromEffFnAff $ runFn4 submitBallotImpl accN encBallot voterPk contract


swmEndTime :: forall e. SwmVotingContract -> Aff (now :: NOW | e) Number
swmEndTime contract = do
    endTimeStr <- ballotPropHelperAff "endTime" noArgs contract
    let endTimeM = fromString endTimeStr
    maybe (throwError $ error $ "endTime could not be converted to a Number: " <> endTimeStr) pure endTimeM


swmNVotes :: forall e. SwmVotingContract -> Aff (| e) Int
swmNVotes contract = do
    nVotesStr <- ballotPropHelperAff "nVotesCast" noArgs contract
    let nVotes = fromStringAs decimal nVotesStr
    maybe (throwError $ error $ "nVotes was not an integer: " <> nVotesStr) pure nVotes


findEthBlockEndingInZeroBefore :: forall e. Int -> Aff (console :: CONSOLE, now :: NOW | e) Int
findEthBlockEndingInZeroBefore targetTime = do
    let initLowBlock = 0
    currBlock <- truncate <$> getBlockNumber
    Tuple currBlockTs lowTs <- sequential $ Tuple <$> parallel (getBlockTimestamp currBlock) <*> parallel (getBlockTimestamp initLowBlock)

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
            Tuple LT _ -> runF {hTs: currBlockTs, hB: currBlock/10, lTs: gHTs, lB: gBH/10}
            -- if lower guess is GT target time
            Tuple _ GT -> runF {hTs: gLTs, hB: gBL/10, lTs: lowTs, lB: initLowBlock/10}
            -- if we hit the money in any way
            Tuple EQ _ -> pure gBH
            Tuple _ EQ -> pure gBL
            -- otherwise we're in between
            Tuple GT LT -> runF {hTs: gHTs, hB: gBH/10, lTs: gLTs, lB: gBL/10}
  where
    _findLastEthBlockBefore :: forall e2. Int -> {hTs :: Int, hB :: Int, lTs :: Int, lB :: Int} -> Aff (console :: CONSOLE | e2) Int
    _findLastEthBlockBefore tTime {hTs, hB, lTs, lB} = do
        case compare hB lB of
                LT -> go tTime lTs lB hTs hB
                EQ -> pure lB
                GT -> go tTime hTs hB lTs lB
      where
        go tTime hTs hB lTs lB = do
            AffC.log $ "Block search: blockN diff: " <> show ((hB*10) - (lB*10)) <> ", Target: " <> show tTime

            let testBlockN = (hB - lB) / 2 + lB
            newTs <- getBlockTimestamp (testBlockN * 10)

            case compare newTs tTime of
                GT -> _findLastEthBlockBefore tTime {hTs: newTs, hB: testBlockN, lTs, lB}
                EQ -> pure $ testBlockN * 10
                LT -> if hB - lB == 1 then pure $ lB * 10 else _findLastEthBlockBefore tTime {hTs, hB, lTs: newTs, lB: testBlockN}
    truncate :: Int -> Int
    truncate a = a - (mod a 10)



legacyVotingAddr :: String
legacyVotingAddr = "0x2bb10945e9f0c9483022dc473ab4951bc2a77d0f"


isLegacy :: String -> Boolean
isLegacy addr = (String.toLower addr) == legacyVotingAddr


runBallotCount :: forall a e.
    Int ->
    String ->
    SwmVotingContract ->
    Erc20Contract ->
    {silent::Boolean} ->
    (StatusUpdate -> Unit) ->
    Aff (now :: NOW, console :: CONSOLE, avar :: AVAR | e) (Either String (Tuple BallotResult AllDetails))
runBallotCount startTime votingAddr contract erc20 {silent} updateF =
      do  -- Aff monad
        nowTime <- liftEff' $ currentTimestamp
        endTime <- swmEndTime contract
        log $ "Ballot end time: " <> (toString endTime) <> "\nCurrent Time:    " <> (toString nowTime)
        if nowTime < endTime
            then pure $ Left "The ballot has not ended yet!"
            else do
                startingBlockFibre <- forkAff $ do
                    optLog $ "Finding Eth block close to time: " <> toStringAs decimal startTime <> " (takes 10-20 seconds)"
                    startBlock <- findEthBlockEndingInZeroBefore startTime
                    optLog $ "Using block " <> show startBlock <> " for ERC20 balances."
                    pure startBlock

                ballotSeckey <- swmBallotSk contract
                optLog $ "Ballot encryption secret key: " <> (toHex $ toUint8Array ballotSeckey)

                nVotes <- swmNVotes contract
                optLog $ "Smart contract reports " <> (intToStr nVotes) <> " votes were cast"

                optLog "Retrieving votes now. This may take some time."
                ballotProgress <- makeVar 0
                encBallotsWithDupes <- getBallots contract nVotes (incrementBallotProgress nVotes optLog ballotProgress)
                optLog $ "Retrieved " <> (lenStr encBallotsWithDupes) <> " votes"
                -- optLog $ "Enc Ballot Order: " <> (show $ map (\{i} -> i) encBallotsWithDupes)

                encBallotsWithoutDupes <- removeDupes encBallotsWithDupes
                optLog $ "Removing repeated votes took nVotes from " <>
                        (intToStr $ length encBallotsWithDupes) <> " to " <>
                        (intToStr $ length encBallotsWithoutDupes)
                -- optLog $ "New Enc Ballot Order: " <> (show $ map (\{i} -> i) encBallotsWithoutDupes)

                decryptedBallots <- decryptBallots ballotSeckey encBallotsWithoutDupes log
                optLog $ "Decrypted " <> (lenStr decryptedBallots) <> " votes successfully"

                -- Note: most Eth nodes do not support historical access due to pruning
                -- in this case the results cannot be verified correctly as we need
                -- access to the balances at the time of the ballot. The below two lines
                -- will use the current block number instead of the historical block number
                -- currentBlockNum <- getBlockNumber
                -- balanceMap <- getBalances erc20 currentBlockNum decryptedBallots

                ballotStartBlock <- joinFiber startingBlockFibre

                balanceMap <- catchError (getBalances erc20 ballotStartBlock decryptedBallots) $ \err -> do
                        let _ = updateF $ mkSUFail $ "Error getting balances: " <> message err
                        throwError err
                optLog $ "Got balances for voters"
                -- optLog $ "Balance Map: \n" <> renderMap balanceMap

                {delegateMap, prefixMap} <- pure $ constructDelegateMap decryptedBallots
                optLog $ "Processed delegates"

                ballotMapPre <- pure $ makeBallotMap decryptedBallots delegateMap
                -- optLog $ "Ballot Map: \n" <> renderMap ballotMapPre

                ballotMap <- pure $ removeBannedAddresses ballotMapPre

                delegateMapNoHang <- pure $ removeHangingDelegations delegateMap ballotMap
                optLog "Removed hanging delegations"

                delegateMapNoLoops <- pure $ removeDelegationLoops delegateMapNoHang
                optLog "Removed delegate loops"

                let parseF = if isLegacy votingAddr then parseBallotOptsLegacy else parseBallotOpts
                ballotOpts <- parseF <$> ballotPropHelperAff "getBallotOptions" noArgs contract
                optLog $ "Got ballot opts: " <> show ballotOpts

                ballotResult <- pure $ countBallots ballotOpts ballotMap delegateMapNoLoops balanceMap

                let allDetails = { ballotSeckey
                                 , nVotes
                                 , encBallotsWithDupes
                                 , encBallotsWithoutDupes
                                 , decryptedBallots
                                 , delegateMap
                                 , delegateMapNoHang
                                 , delegateMapNoLoops
                                 , ballotMap
                                 , ballotResult
                                 , balanceMap
                                 }

                pure $ Right (Tuple ballotResult allDetails)

    where
        log :: forall e. String -> Aff (console :: CONSOLE | e) Unit
        log str = do
            let _ = updateF $ mkSULog str
            if silent then pure unit else AffC.log str
        lenStr :: forall a. Array a -> String
        lenStr = intToStr <<< length
        optLog = log


-- | Log and increment the number of ballots we've processed to faciliate progress updates
incrementBallotProgress :: forall e. Int -> (String -> Aff (console :: CONSOLE, avar :: AVAR | e) Unit) -> AVar Int -> Aff (avar :: AVAR, console :: CONSOLE | e) Unit
incrementBallotProgress totalBallots log avar = do
    n <- (+) 1 <$> takeVar avar
    putVar n avar
    if n `mod` 10 == 0
        then log $ "Processed " <> show n <> " ballots; " <> show (n * 100 / totalBallots) <> "% done."
        else pure unit



getBallots :: forall e. SwmVotingContract -> Int -> Aff (console :: CONSOLE, avar :: AVAR | e) Unit -> Aff (console :: CONSOLE, avar :: AVAR | e) (Array EncBallot)
getBallots contract n incBallotProgress
    | n <= 0 = pure []
    | otherwise = do
        let allVoteIds = range 0 (n-1)
        let (chunks :: Array (Array Int)) = chunk 10 allVoteIds
        let (toPar :: Array (Aff _ (Array EncBallot))) = (parTraverse getBallot) <$> chunks
        map concat $ sequence $ toPar
    where
        getBallot i = do
            encBallot <- keyFromEthHex $ ballotPropHelperAff "encryptedBallots" [i] contract
            voterPk <- keyFromEthHex $ ballotPropHelperAff "associatedPubkeys" [i] contract
            voterAddrStr <- ballotPropHelperAff "associatedAddresses" [i] contract
            voterAddr <- maybe (throwError $ error $ "Got bad address from web3: " <> voterAddrStr) pure (toVoter <$> toAddress voterAddrStr)
            incBallotProgress
            pure $ {i, encBallot, voterPk, voterAddr}
        keyFromEthHex k = do
            ethHex <- k
            either (throwError <<< error) pure (fromEthHexE ethHex)


-- todo, this is going to be slow -- might be much faster to reverse the array...
-- implemented as per 4.9.3 in Swarm Voting Spec
removeDupes :: forall e. Array EncBallot -> Aff (| e) (Array EncBallot)
removeDupes [] = pure []
removeDupes encBallots = do
        lastBallot <- maybe (throwError $ error "encBallots did not have a `last` element - but we checked for that!") pure $ last encBallots
        let remBallots = filter (\b -> b.voterAddr /= lastBallot.voterAddr) encBallots
        (<>) <$> (removeDupes remBallots) <*> pure [lastBallot]



decryptBallots :: forall e e2. BoxSecretKey -> Array EncBallot -> (String -> Aff (console :: CONSOLE | e) Unit) -> Aff (console :: CONSOLE | e) (Array (Ballot))
decryptBallots _ [] log = pure []
decryptBallots encSk ballots log = do
        ballots_ <- parTraverse decryptOne ballots
        let cleanBallots = map (unsafePartial $ fromRight) (filter isRight ballots_)
        let badDecryptions = map (unsafePartial $ fromLeft) (filter isLeft ballots_)
        if length badDecryptions > 0 then
                log $ "Got " <> (show $ length badDecryptions) <> " bad decryptions: " <> show badDecryptions
            else
                log "All ballots decrypted successfully."
        pure cleanBallots
    where
        decryptOne {i, encBallot, voterPk, voterAddr} = do
            let ballotM = toUint8Array <$> (decryptOneTimeBallot (toBox encBallot) (toBoxPubkey voterPk) encSk)
            maybe (pure $ Left $ "Cannot decrypt ballot from: " <> show voterAddr) (\ballot -> pure $ Right {ballot, voterPk, voterAddr}) ballotM


getBalances :: forall e. Erc20Contract -> Int -> Array Ballot -> Aff (| e) BalanceMap
getBalances erc20 blockNumber ballots = do
        pairs <- parTraverse addBalance ballots
        pure $ fromFoldable pairs
    where
        addBalance {ballot, voterPk, voterAddr} = do
            balM <- Dec.fromString <$> ballotPropHelperWBlockNumAff "balanceOf" [voterAddr] blockNumber erc20
            balance <- maybe (throwError $ error $ "Unable to get balance for " <> show voterAddr) pure balM
            pure $ Tuple voterAddr balance


constructDelegateMap :: Array Ballot -> {delegateMap :: DelegateMap, prefixMap :: Map String Voter}
constructDelegateMap ballots =
        {delegateMap, prefixMap: addrPrefixMap}
    where
        delegateMap =
            foldl (\m {voterAddr, delegateAddr} -> insert voterAddr delegateAddr m) empty voterDelegateParis

        voterDelegateParis =
            map (\{voterAddr, ballot} -> {voterAddr, delegateAddr: genDelegateFromBallot ballot}) ballots

        genDelegateFromBallot ballotUI8A =
            lookup (drop 4 $ toHex ballotUI8A) addrPrefixMap

        addrPrefixMap =
            foldl (\m addr -> insert (String.take (14*2) $ drop 2 $ voterToString addr) addr m) empty allAddresses

        allAddresses =
            map (\{voterAddr} -> voterAddr) ballots


-- Note: here we remove all *values* from delMap if the lookup
-- results in a (Nothing) or a (Just (Left _)) in ballotMap
removeHangingDelegations :: forall k1 v. Show k1 => Ord k1 =>
        Map k1 (Maybe k1) -> Map k1 (Either String v) -> Map k1 (Maybe k1)
removeHangingDelegations delMap ballotMap = removeInvalidDelegates delMap validDelegates
    where
        validDelegates =
            (filter (\k -> isRight $ maybe (Left "") (join <<< Right) $ lookup k ballotMap)
                $ fromList $ keys ballotMap)
        removeInvalidDelegates dMap validDlgts = foldl (remDelegatesFoldF validDlgts) dMap (fromList $ keys dMap)
        remDelegatesFoldF validDlgts dMapFolding voter = do
            let theirDlgtM = join $ lookup voter dMapFolding
            -- if their delegate is valid, return the delegate map, otherwise insert `Nothing` as their delegate
            case theirDlgtM of
                Nothing -> dMapFolding  -- nothing to do here
                (Just theirDlgt) -> if theirDlgt `elem` validDlgts then dMapFolding else insert voter Nothing dMapFolding


removeDelegationLoops :: forall k. Show k => Ord k => Map k (Maybe k) -> Map k (Maybe k)
removeDelegationLoops dMap = foldl checkForLoops dMap $ fromList $ keys dMap
    where
        checkForLoops m voter = do
            let delegationChain = mkDelegationChain m voter []
            let lastInDelegationChian = last delegationChain
            case lastInDelegationChian of
                -- if we have no last element then there was no delegations; this should never happen
                Nothing -> unsafeCrashWith "Delegate loop check should always have a delegation list of length >= 1"
                (Just finalDelegate) -> case (init delegationChain) of
                    Nothing -> m  -- if we have no `init` of the delegationChain then it only had 1 elem
                    (Just allButLast) -> if finalDelegate `elem` allButLast
                        then purgeLoopFrom m finalDelegate delegationChain
                        else m  -- if the final delegate wasn't in the list of delegates we don't need to do anything
        mkDelegationChain m voter pastChain = case (join $ lookup voter m) of
                Nothing -> currChain  --
                Just (next) -> do
                    if next `elem` currChain
                        then currChain <> [next]  -- terminate as we've found the first elem that we've seen before
                        else mkDelegationChain m next (currChain)
            where currChain = pastChain <> [voter]

        purgeLoopFrom m delegate dChain = do
            let nextMaybe = join $ lookup delegate m
            let mNext = insert delegate Nothing m
            case nextMaybe of
                Nothing -> mNext
                (Just next) -> purgeLoopFrom mNext next dChain



makeBallotMap :: Array Ballot -> DelegateMap -> BallotMap
makeBallotMap ballots delegateMap = foldl insertSwmBallot empty $ arrSwmBs
    where
        arrSwmBs = map mkSwmBallot ballots
        mkSwmBallot :: Ballot -> Maybe {voterAddr::Voter, swmBallot::Either String SwmBallot}
        mkSwmBallot {ballot, voterPk, voterAddr} = do
            bVotes <- ballotVotes ballot
            pure $ {voterAddr, swmBallot: toSwmBallot voterAddr bVotes}
        ballotVotes :: Uint8Array -> Maybe (Array Int)
        ballotVotes ballot = do
            let ballotBytes = SVAB.take 2 ballot
            let ballotBitStr = foldl (<>) "" $ map intByteToBitStr $ toIntArray ballotBytes
            -- this is where we choose how many bits to take and thus how many ballot options there are
            let ballotBits = String.take 15 ballotBitStr
            let ballotBitArray = splitBits 3 ballotBits
            let offsetBallotA = sequence $ (fromStringAs binary) <$> ballotBitArray
            (map (\n -> n - 3)) <$> offsetBallotA  -- we store ballots in the range [0,6] so we need to offset them back
        splitBits _ "" = []
        splitBits n str = (String.take n str) : splitBits n (String.drop n str)
        insertSwmBallot m Nothing = m
        insertSwmBallot m (Just {voterAddr, swmBallot}) = insert voterAddr swmBallot m


removeBannedAddresses :: BallotMap -> BallotMap
removeBannedAddresses ballotMapPre =
        foldl (\m bannedAddr -> delete bannedAddr m) ballotMapPre bannedAddresses


renderMap :: forall k v. (Ord k) => (Show k) => (Show v) => Map k v -> String
renderMap m = joinWith "\n" $ map (\{k, v} -> "  " <> show k <> "\n   \\-> " <> v <> "\n") kvPairs
    where
        kvPairs = map (\k -> {k, v: maybe "Nothing" (\a -> show a) $ lookup k m}) $ fromList $ keys m


parseBallotOptsLegacy :: String -> OptStrs
parseBallotOptsLegacy rawBallotOpts = if length firstSplit /= 8
        then OptStrs {o1: totalErr, o2: "", o3: "", o4: "", o5: ""}
        else renderedOpts
    where
        firstSplit = String.split (Pattern ",") rawBallotOpts
        pairs [] = []
        pairs strs = (Tuple (A.head strs) (A.head $ A.drop 1 strs)) : pairs (A.drop 2 strs)
        formatTuple t = case t of
            (Tuple (Just nRs) (Just days)) -> nRs <> " releases of " <> days <> " days each"
            _ -> decodingErr
        optsArr = map formatTuple (pairs firstSplit)
        renderedOpts = OptStrs {o1: getOpt 1 optsArr, o2: getOpt 2 optsArr, o3: getOpt 3 optsArr, o4: getOpt 4 optsArr, o5: ""}
        jOrErr i strM = fromMaybe ("Unable to decode opt " <> intToStr i) strM
        getOpt i arr = jOrErr i $ A.head $ A.drop (i-1) arr
        totalErr = "Incorrect number of options returned: " <> rawBallotOpts
        decodingErr = "Unable to decode option"


parseBallotOpts :: String -> OptStrs
parseBallotOpts rawBallotOpts = if length firstSplit /= maxOptions
        then OptStrs {o1: totalErr, o2: "", o3: "", o4: "", o5: ""}
        else renderedOpts
    where
        firstSplit = String.split (Pattern ",") rawBallotOpts
        optsArr = firstSplit
        renderedOpts = OptStrs {o1: getOpt 1 optsArr, o2: getOpt 2 optsArr, o3: getOpt 3 optsArr, o4: getOpt 4 optsArr, o5: getOpt 5 optsArr}
        jOrErr i strM = fromMaybe ("Unable to decode opt " <> intToStr i) strM
        getOpt i arr = jOrErr i $ A.head $ A.drop (i-1) arr
        totalErr = "Incorrect number of options returned: " <> rawBallotOpts
        decodingErr = "Unable to decode option"
        maxOptions = 5


countBallots :: forall e. OptStrs -> BallotMap -> DelegateMap -> BalanceMap -> BallotResult
countBallots (OptStrs {o1, o2, o3, o4, o5}) ballotMap delegateMap balanceMap = do
        -- first, find the ballot to use for each voter (i.e. account for delegations)
        let voters = fromList $ keys ballotMap
        let processedBallotMap = findBallotsOfMostDelegated voters

        -- usually we'd multiply the ballots by their reputation,
        -- but this is just their ballance for this ballot
        -- so let's get a list of ballots and multiples (which are the balances)
        let voterBallotPairs = (toUnfoldable processedBallotMap :: Array (Tuple Voter (Either String SwmBallot)))
        let balanceBallotPairs = (\(Tuple voter ballot) -> (toBalanceBallotPair voter ballot)) <$> voterBallotPairs

        -- now we need to extract lists of scaled ballot
        let {v1s, v2s, v3s, v4s, v5s} = procBalanceBallotPairs balanceBallotPairs

        -- now we sum them all
        let totals =
                [ Tuple o1 $ sum v1s
                , Tuple o2 $ sum v2s
                , Tuple o3 $ sum v3s
                , Tuple o4 $ sum v4s
                , Tuple o5 $ sum v5s
                ]

        -- and that's it! We pick the maximum and that's our winner.
        -- we better make sure there wasn't a tie though :)
        let ourWinners = onlyWinners totals

        let winner = if length ourWinners == 1 then head ourWinners else Nothing
        let possibleWinners = if length ourWinners > 1 then ourWinners else []

        {winner, possibleWinners, totals}

    where
        findBallotsOfMostDelegated voters =
            foldl (\m voter -> fromMaybe empty $ do
                finalDlgt <- pure $ followDelegationChain voter
                dlgtBallot <- lookup finalDlgt ballotMap
                pure $ insert voter dlgtBallot m
            ) ballotMap voters

        followDelegationChain voter = case (join $ lookup voter delegateMap) of
            Nothing -> voter
            (Just next) -> followDelegationChain next

        toBalanceBallotPair :: Voter -> Either String SwmBallot -> Either String BalanceBallotPair
        toBalanceBallotPair voter ballotE = do
            ballot <- ballotE
            balance <- maybe (Left $ "Unable to get balance for: " <> show voter) Right $ lookup voter balanceMap
            pure $ {balance, ballot: getVotes ballot}

        procBalanceBallotPairs :: Array (Either String BalanceBallotPair) -> ScaledVotes
        procBalanceBallotPairs allPairs = foldl makeScaledVotes {v1s: [], v2s: [], v3s: [], v4s: [], v5s: []} allPairs

        makeScaledVotes :: ScaledVotes -> Either String BalanceBallotPair -> ScaledVotes
        makeScaledVotes scaledVotes (Left err) = do
            let _ = unsafePerformEff $ EffC.log $ "Error while counting ballots: " <> err
            scaledVotes
        makeScaledVotes {v1s, v2s, v3s, v4s, v5s} (Right {balance, ballot}) = do
            let {v1, v2, v3, v4, v5} = ballot
            let s = ((*) balance <<< Dec.fromInt <<< voteToInt)
            {v1s: (s v1):v1s, v2s: (s v2):v2s, v3s: (s v3):v3s, v4s: (s v4):v4s, v5s: (s v5):v5s}

        onlyWinners totals = getMaxTotals totals []

        getMaxTotals totals currBests = do
            case Tuple (head totals) (tail totals) of
                (Tuple Nothing _) -> currBests
                (Tuple _ Nothing) -> currBests
                (Tuple (Just aTotal@(Tuple opt score)) (Just remTotals)) ->
                    case head currBests of
                        Nothing -> getMaxTotals remTotals [aTotal]
                        (Just (Tuple prevOpt prevScore)) -> case compare score prevScore of
                            GT -> getMaxTotals remTotals [aTotal]
                            EQ -> getMaxTotals remTotals (currBests <> [aTotal])
                            LT -> getMaxTotals remTotals currBests
