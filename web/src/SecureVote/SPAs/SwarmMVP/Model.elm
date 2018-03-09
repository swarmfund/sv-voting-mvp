module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import List exposing (foldl, map)
import Material
import Material.Snackbar
import Maybe.Extra exposing ((?))
import Monocle.Common exposing ((=>), dict, maybe)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import RemoteData exposing (RemoteData(..))
import SecureVote.Ballots.Types exposing (BallotSCDetails, BallotSpec)
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Model exposing (EthMdl, initEthMdl)
import SecureVote.Eth.Nodes exposing (ethNodes)
import SecureVote.Eth.Types exposing (..)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (allBallots, initBallot)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(NotFoundDialog), Route(ListAllVotesR, NotFoundR, SwmAddressR))
import SecureVote.SPAs.SwarmMVP.Types exposing (Flags, TxidCheckStatus(TxidNotMade))
import SecureVote.Utils.Lenses exposing ((=|>))
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits)


type alias Model =
    { mdl : Material.Model
    , snack : Material.Snackbar.Model String
    , errors : List String
    , dialogHtml : { title : String, route : DialogRoute Msg }
    , elevations : Dict Int Bool
    , fields : Dict String String
    , boolFields : Dict String Bool
    , ballotRange : Dict Int Int
    , ballotBits : Dict Int (Result String RangeBallot3Bits)
    , ballotAllDone : Bool
    , currentBallot : Maybe String
    , allBallots : Dict Int (BallotParams Msg)
    , route : Route
    , history : List Route
    , lastPageDirection : LastPageDirection
    , lastRoute : Maybe Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    , keypair : Maybe Curve25519KeyPair
    , encBytes : Maybe String
    , ballotPlaintext : Result String (List Int)
    , ballotRawHex : Maybe String
    , remoteHexPk : Maybe String
    , miniVotingAbi : String
    , verificationError : Maybe String
    , ballotVerificationPassed : RemoteData String Bool
    , txidCheck : TxidCheckStatus
    , ballotOpen : RemoteData String { startTime : Int, endTime : Int }
    , now : Int
    , mainTitle : String
    , auditMsgs : List AuditDoc
    , dev : Bool
    , optHashToTitle : Dict String String
    , metamask : Bool
    , metamaskTxid : Maybe String
    , democHashes : Dict Int String --^ Map (index order => democHash)
    , democCounts : Dict String Int --^ map (democHashes => number of ballots in it)
    , democIToSpec : Dict String (Dict Int String) --^ map (democHash => (ballotId => prelimInfo))
    , democIssues : Dict String (Dict String BallotPrelimInfo) --^ map (democHash => (ballotId => prelimInfo))
    , specToDeets : Dict String BallotSpec --^ map (specHash => RemoteData BallotSpec) - can error gracefully
    , haveVotedOn : Dict String (Dict String Bool) --^ (voterAddr => (specHash => Have Voted On))
    , pendingVotes : Dict String (Dict String Float) --^ (voterAddr => (bHash => Bool))
    , failedSpec : Dict String String
    , fatalSpecFail : List String
    , currDemoc : String
    , erc20Abrvs : Dict String String --^ map (bHash => erc20Abrv)
    , erc20Balance : Maybe Decimal
    , indexABI : String
    , ballotBoxABI : String
    , ballotScDetails : Dict String BallotSCDetails --^ map (bHash => BallotSCDetails)
    , delegationABI : String
    , delegationAddr : String
    , lsBucket : Dict String String
    , web3 : EthMdl
    }


mErc20Abrv : String -> Lens Model String
mErc20Abrv bHash =
    Lens
        (.erc20Abrvs >> (dict bHash).getOption >> Maybe.withDefault "ERC20")
        (\abrv m -> { m | erc20Abrvs = (dict bHash).set abrv m.erc20Abrvs })


mBSpec : String -> Optional Model BallotSpec
mBSpec bHash =
    Optional
        (.specToDeets >> (dict bHash).getOption)
        (\bSpec m -> { m | specToDeets = (dict bHash).set bSpec m.specToDeets })


mVotingAddr : String -> Optional Model String
mVotingAddr bHash =
    Optional
        (\m -> (dict m.currDemoc => dict bHash =|> bpiVotingAddr).getOption m.democIssues)
        (\a m -> { m | democIssues = (dict m.currDemoc => dict bHash =|> bpiVotingAddr).set a m.democIssues })


mCurrVotingAddr : Lens Model String
mCurrVotingAddr =
    Lens
        (\m -> (m.currentBallot |> Maybe.andThen (\h -> (mVotingAddr h).getOption m)) ? "NO ADDRESS FOR CURRENT VOTE")
        (\a m -> (m.currentBallot |> Maybe.map (\h -> (mVotingAddr h).set a m)) ? m)


mBHashBSpecPair : Model -> Maybe ( String, BallotSpec )
mBHashBSpecPair model =
    case model.currentBallot of
        Just bHash ->
            Maybe.map (\bSpec -> ( bHash, bSpec )) (Dict.get bHash model.specToDeets)

        _ ->
            Nothing


type alias BallotPrelimInfo =
    { specHash : String, votingContract : String, extraData : String, startTime : Int }


bpiStartTime : Lens BallotPrelimInfo Int
bpiStartTime =
    Lens .startTime (\i b -> { b | startTime = i })


bpiVotingAddr : Lens BallotPrelimInfo String
bpiVotingAddr =
    Lens .votingContract (\s b -> { b | votingContract = s })


initModel : Flags -> Model
initModel { dev, mainTitle, democHash, ballotBoxABI, indexABI, delegationABI, delegationAddr } =
    let
        ethNode_ =
            if dev then
                ethNodes.kovan
            else
                ethNodes.mainnet
    in
    { mdl = Material.model
    , snack = Material.Snackbar.model
    , errors = []
    , dialogHtml = { title = "Error: Dialog has not been updated.", route = NotFoundDialog }
    , elevations = Dict.empty
    , fields = Dict.empty
    , boolFields = Dict.empty
    , ballotRange = Dict.empty
    , ballotBits = Dict.empty
    , ballotAllDone = False
    , currentBallot = Nothing
    , allBallots = Dict.empty
    , route = SwmAddressR
    , history = []
    , lastRoute = Nothing
    , lastPageDirection = PageForward
    , candidateTx = nullCandidateEthTx
    , ethNode = ethNode_
    , keypair = Nothing
    , encBytes = Nothing
    , ballotPlaintext = Err "Not initialized yet"
    , ballotRawHex = Nothing
    , remoteHexPk = Nothing
    , miniVotingAbi = "Error: Web3 has not initialized correctly"
    , verificationError = Nothing
    , ballotVerificationPassed = Loading
    , txidCheck = TxidNotMade
    , ballotOpen = Loading
    , now = 0
    , mainTitle = mainTitle
    , auditMsgs = []
    , dev = dev
    , optHashToTitle = Dict.empty
    , metamask = False
    , metamaskTxid = Nothing
    , democHashes = Dict.empty
    , democCounts = Dict.empty
    , democIToSpec = Dict.empty
    , democIssues = Dict.empty
    , specToDeets = Dict.empty
    , haveVotedOn = Dict.empty
    , pendingVotes = Dict.empty
    , failedSpec = Dict.empty
    , fatalSpecFail = []
    , currDemoc = democHash
    , erc20Abrvs = Dict.empty
    , erc20Balance = Nothing
    , indexABI = indexABI
    , ballotBoxABI = ballotBoxABI
    , ballotScDetails = Dict.empty
    , delegationABI = delegationABI
    , delegationAddr = delegationAddr
    , lsBucket = Dict.empty
    , web3 = initEthMdl
    }


type LastPageDirection
    = PageForward
    | PageBack
