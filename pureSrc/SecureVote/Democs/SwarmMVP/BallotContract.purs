module SecureVote.Democs.SwarmMVP.BallotContract where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff (Aff, error, launchAff, parallel, sequential, throwError)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxPublicKey, BoxSecretKey, toUint8Array)
import Data.Array (elem, filter, foldl, head, init, last, length, range, replicate, sortBy, tail, takeWhile, zip, (:))
import Data.Array as A
import Data.ArrayBuffer.Typed (toIntArray)
import Data.ArrayBuffer.Types (ArrayView, Uint8, Uint8Array)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Decimal (Decimal)
import Data.Decimal as Dec
import Data.Either (Either(..), either, isRight)
import Data.Function.Uncurried (Fn0, Fn1, Fn2, Fn3, Fn4, Fn5, Fn6, runFn0, runFn1, runFn2, runFn3, runFn4, runFn5, runFn6)
import Data.Int (binary, decimal, fromStringAs, toNumber, toStringAs)
import Data.Int as DInt
import Data.Map (Map, delete, empty, fromFoldable, insert, keys, lookup, showTree, size, values, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (Pattern(..), drop, joinWith)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (maximumBy, sequence, sum, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Math (round)
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import SecureVote.Crypto.Curve25519 (decryptOneTimeBallot, toBox, toBoxPubkey, toBoxSeckey)
import SecureVote.Democs.SwarmMVP.Admin (main)
import SecureVote.Democs.SwarmMVP.Ballot (delegateAddr)
import SecureVote.Democs.SwarmMVP.BannedAddrs (bannedAddresses)
import SecureVote.Democs.SwarmMVP.Const (balanceAt)
import SecureVote.Democs.SwarmMVP.Types (Address, Delegate, SwmBallot, Vote, Voter, Votes, VotesRecord, getVoter, getVotes, toAddress, toDelegate, toSwmBallot, toVoter, voteToInt, voterToString)
import SecureVote.Utils.Array (fromList)
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
type ScaledVotes = {v1s :: Array Decimal, v2s :: Array Decimal, v3s :: Array Decimal, v4s :: Array Decimal}


newtype OptStrs = OptStrs {o1::String, o2::String, o3::String, o4::String}
instance optStrShow :: Show OptStrs where
    show (OptStrs {o1, o2, o3, o4}) = String.joinWith ", " [o1,o2,o3,o4]


noArgs :: forall a. Array a
noArgs = []


-- contract setup FFI
foreign import makeSwmVotingContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe SwmVotingContract)
foreign import makeErc20ContractImpl :: forall a. Fn3 (a -> Maybe a) (Maybe a) String (Maybe Erc20Contract)
foreign import setWeb3ProviderImpl :: forall a. Fn2 String String a

-- web3 FFI
foreign import getAccountImpl :: forall a b. Fn3 (a -> Either a b) (b -> Either a b) Int (Either String String)
foreign import getBlockNumberImpl :: forall e. Fn0 (EffFnAff (| e) Int)

getBlockNumber :: forall eff. Aff (| eff) Int
getBlockNumber = fromEffFnAff (runFn0 getBlockNumberImpl)

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
makeSwmVotingContract :: forall a. String -> Maybe SwmVotingContract
makeSwmVotingContract = runFn3 makeSwmVotingContractImpl Just Nothing

makeErc20Contract :: forall a. String -> Maybe Erc20Contract
makeErc20Contract = runFn3 makeErc20ContractImpl Just Nothing


setWeb3Provider :: forall a. String -> String -> a 
setWeb3Provider = runFn2 setWeb3ProviderImpl

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


swmEndTime :: forall e. SwmVotingContract -> Aff (| e) Number
swmEndTime contract = do
    endTimeStr <- ballotPropHelperAff "endTime" noArgs contract
    let endTimeM = fromString endTimeStr
    maybe (throwError $ error $ "endTime could not be converted to a Number: " <> endTimeStr) pure endTimeM


swmNVotes :: forall e. SwmVotingContract -> Aff (| e) Int
swmNVotes contract = do
    nVotesStr <- ballotPropHelperAff "nVotesCast" noArgs contract
    let nVotes = fromStringAs decimal nVotesStr
    maybe (throwError $ error $ "nVotes was not an integer: " <> nVotesStr) pure nVotes


runBallotCount :: forall a e. Int -> SwmVotingContract -> Erc20Contract -> {silent::Boolean} -> (Aff (now :: NOW, console :: CONSOLE | e) (Either String BallotResult))
runBallotCount ballotStartBlock contract erc20 {silent} = 
      do  -- Aff monad
        nowTime <- liftEff $ currentTimestamp
        endTime <- swmEndTime contract 
        log $ "Ballot end time: " <> (toString endTime) <> "\nCurrent Time:    " <> (toString nowTime)
        if nowTime < endTime 
            then pure $ Left "The ballot has not ended yet!"
            else do
                ballotSeckey <- swmBallotSk contract
                optLog $ "Ballot encryption secret key: " <> (toHex $ toUint8Array ballotSeckey)

                nVotes <- swmNVotes contract 
                optLog $ "Smart contract reports " <> (intToStr nVotes) <> " votes were cast"

                encBallotsWithDupes <- getBallots contract nVotes
                optLog $ "Retrieved " <> (lenStr encBallotsWithDupes) <> " ballots"
                -- optLog $ "Enc Ballot Order: " <> (show $ map (\{i} -> i) encBallotsWithDupes)

                encBallotsWithoutDupes <- removeDupes encBallotsWithDupes
                optLog $ "Removing repeated votes took nBallots from " <> 
                        (intToStr $ length encBallotsWithDupes) <> " to " <>
                        (intToStr $ length encBallotsWithoutDupes)
                -- optLog $ "New Enc Ballot Order: " <> (show $ map (\{i} -> i) encBallotsWithoutDupes)

                decryptedBallots <- decryptBallots ballotSeckey encBallotsWithoutDupes
                optLog $ "Decrypted " <> (lenStr decryptedBallots) <> " ballots successfully"

                balanceMap <- getBalances erc20 ballotStartBlock decryptedBallots
                optLog $ "Got balances for voters"
                -- log $ "Balance Map: \n" <> renderMap balanceMap

                {delegateMap, prefixMap} <- pure $ constructDelegateMap decryptedBallots
                optLog $ "Processed delegates"

                ballotMapPre <- pure $ makeBallotMap decryptedBallots delegateMap
                -- optLog $ "Ballot Map: \n" <> renderMap ballotMapPre

                ballotMap <- pure $ removeBannedAddresses ballotMapPre

                delegateMapNoHang <- pure $ removeHangingDelegations delegateMap ballotMap
                optLog "Removed hanging delegations"

                delegateMapNoLoops <- pure $ removeDelegationLoops delegateMapNoHang
                optLog "Removed delegate loops"

                ballotOpts <- parseBallotOpts <$> ballotPropHelperAff "getBallotOptions" noArgs contract 
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
                                 }
                
                pure $ Right ballotResult
                    
    where
        lenStr :: forall a. Array a -> String 
        lenStr = intToStr <<< length
        optLog str = if silent then pure unit else log str


getBallots :: forall e. SwmVotingContract -> Int -> Aff (| e) (Array EncBallot)
getBallots contract n 
    | n <= 0 = pure []
    | otherwise = do
        let allVoteIds = range 0 (n-1)
        parTraverse getBallot allVoteIds
    where
        getBallot i = do
            encBallot <- keyFromEthHex $ ballotPropHelperAff "encryptedBallots" [i] contract
            voterPk <- keyFromEthHex $ ballotPropHelperAff "associatedPubkeys" [i] contract
            voterAddrStr <- ballotPropHelperAff "associatedAddresses" [i] contract
            voterAddr <- maybe (throwError $ error "Got bad address from web3") pure (toVoter <$> toAddress voterAddrStr)
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



decryptBallots :: forall e. BoxSecretKey -> Array EncBallot -> Aff (| e) (Array Ballot)
decryptBallots _ [] = pure []
decryptBallots encSk ballots = do
        parTraverse decryptOne ballots
    where
        decryptOne {i, encBallot, voterPk, voterAddr} = do
            let ballotM = toUint8Array <$> (decryptOneTimeBallot (toBox encBallot) (toBoxPubkey voterPk) encSk)
            maybe (throwError $ error $ "Unable to decrypt ballot from: " <> show voterAddr) (\ballot -> pure $ {ballot, voterPk, voterAddr}) ballotM


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
            let ballotBits = String.take 12 ballotBitStr
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


parseBallotOpts :: String -> OptStrs
parseBallotOpts rawBallotOpts = if length firstSplit /= 8 
        then OptStrs {o1: totalErr, o2: totalErr, o3: totalErr, o4: totalErr}
        else renderedOpts
    where
        firstSplit = String.split (Pattern ",") rawBallotOpts
        pairs [] = []
        pairs strs = (Tuple (A.head strs) (A.head $ A.drop 1 strs)) : pairs (A.drop 2 strs)
        formatTuple t = case t of
            (Tuple (Just nRs) (Just days)) -> nRs <> " releases of " <> days <> " days each"
            _ -> decodingErr
        optsArr = map formatTuple (pairs firstSplit)
        renderedOpts = OptStrs {o1: getOpt 1 optsArr, o2: getOpt 2 optsArr, o3: getOpt 3 optsArr, o4: getOpt 4 optsArr}
        jOrErr i strM = fromMaybe ("Unable to decode opt " <> intToStr i) strM
        getOpt i arr = jOrErr i $ A.head $ A.drop (i-1) arr
        totalErr = "Incorrect number of options returned: " <> rawBallotOpts
        decodingErr = "Unable to decode option"


countBallots :: forall e. OptStrs -> BallotMap -> DelegateMap -> BalanceMap -> BallotResult
countBallots (OptStrs {o1, o2, o3, o4}) ballotMap delegateMap balanceMap = do
        -- first, find the ballot to use for each voter (i.e. account for delegations)
        let voters = fromList $ keys ballotMap
        let processedBallotMap = findBallotsOfMostDelegated voters

        -- usually we'd multiply the ballots by their reputation, 
        -- but this is just their ballance for this ballot
        -- so let's get a list of ballots and multiples (which are the balances)
        let voterBallotPairs = (toUnfoldable processedBallotMap :: Array (Tuple Voter (Either String SwmBallot)))
        let balanceBallotPairs = (\(Tuple voter ballot) -> (toBalanceBallotPair voter ballot)) <$> voterBallotPairs
        
        -- now we need to extract lists of scaled ballot
        let {v1s, v2s, v3s, v4s} = procBalanceBallotPairs balanceBallotPairs

        -- now we sum them all
        let totals = 
                [ Tuple o1 $ sum v1s
                , Tuple o2 $ sum v2s
                , Tuple o3 $ sum v3s
                , Tuple o4 $ sum v4s 
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
        procBalanceBallotPairs allPairs = foldl makeScaledVotes {v1s: [], v2s: [], v3s: [], v4s: []} allPairs
        
        makeScaledVotes :: ScaledVotes -> Either String BalanceBallotPair -> ScaledVotes
        makeScaledVotes scaledVotes (Left err) = do
            let _ = unsafePerformEff $ EffC.log $ "Error while counting ballots: " <> err
            scaledVotes
        makeScaledVotes {v1s, v2s, v3s, v4s} (Right {balance, ballot}) = do
            let {v1, v2, v3, v4} = ballot
            let s = ((*) balance <<< Dec.fromInt <<< voteToInt)
            {v1s: (s v1):v1s, v2s: (s v2):v2s, v3s: (s v3):v3s, v4s: (s v4):v4s}

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




