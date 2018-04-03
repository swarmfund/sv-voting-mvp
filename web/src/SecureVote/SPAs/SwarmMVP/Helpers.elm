module SecureVote.SPAs.SwarmMVP.Helpers exposing (..)

import Date
import Date.Extra.Config.Config_en_us as USDate
import Date.Extra.Format exposing (format, isoString)
import Dict
import Dict.Extra
import Html exposing (Html, pre)
import Html.Attributes exposing (class)
import Maybe exposing (andThen)
import Maybe.Extra
import Monocle.Common exposing (dict)
import ParseInt exposing (parseIntRadix, toRadix)
import RemoteData exposing (RemoteData(Loading))
import Result.Extra exposing (isOk)
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Crypto.Hashing exposing (hashToInt)
import SecureVote.Eth.Types exposing (nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Fields exposing (..)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, mBSpec)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(TxidNotMade))
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits, intsToRangeBallot3Bits)


resetAllBallotFields : Model -> { b | contractAddr : String } -> Model
resetAllBallotFields model { contractAddr } =
    { model
        | ballotRange = Dict.empty
        , ballotBits = Dict.empty
        , ballotAllDone = False
        , candidateTx = { nullCandidateEthTx | to = Just contractAddr, from = getUserErc20Addr model }
        , encBytes = Nothing
        , ballotPlaintext = Err "Ballot fields reset and ballotPlaintext not set yet."
        , remoteHexPk = Nothing
        , miniVotingAbi = "Error: Ballot parameters have been reset and ABI is not set yet."
        , ballotVerificationPassed = Loading
        , txidCheck = TxidNotMade
        , ballotOpen = Loading
        , auditMsgs = []
        , metamaskTxid = Nothing
    }


getNBallotsVotedOn : Model -> Int
getNBallotsVotedOn model =
    getUserErc20Addr model
        |> Maybe.andThen (\a -> (dict a).getOption model.haveVotedOn)
        |> Maybe.map (Dict.filter (\k v -> v))
        |> Maybe.map Dict.size
        |> Maybe.withDefault 0


getNBallots : Model -> Int
getNBallots model =
    (dict model.currDemoc).getOption model.democIssues
        |> Maybe.map Dict.size
        |> Maybe.withDefault 0


getLiveBallots : Model -> Dict.Dict String Bool
getLiveBallots model =
    (dict model.currDemoc).getOption model.democIssues
        |> Maybe.withDefault Dict.empty
        |> Dict.Extra.filterMap
            (\k { specHash } ->
                (mBSpec specHash).getOption model
            )
        |> Dict.Extra.filterMap
            (\k bSpec ->
                case ( bStartTime.getOption bSpec, bEndTime.getOption bSpec ) of
                    ( Just st, Just et ) ->
                        if st < model.now && model.now < et then
                            Just True
                        else
                            Nothing

                    _ ->
                        Nothing
            )


getNLiveBallots : Model -> Int
getNLiveBallots model =
    getLiveBallots model |> Dict.size


getNLiveBallotsVotedOn : Model -> Int
getNLiveBallotsVotedOn model =
    let
        liveBallots =
            getLiveBallots model
    in
    getUserErc20Addr model
        |> Maybe.andThen (\a -> (dict a).getOption model.haveVotedOn)
        |> Maybe.withDefault Dict.empty
        |> Dict.filter (\k v -> v && (Maybe.Extra.isJust <| Dict.get k liveBallots))
        |> Dict.size


getUserErc20Addr : Model -> Maybe String
getUserErc20Addr model =
    Dict.get userErc20AddrId model.fields


setUserErc20Addr : String -> Msg
setUserErc20Addr s =
    let
        s_ =
            String.toLower s
    in
    MultiMsg [ SetField userErc20AddrId s_, SetCandidateTx (\tx -> { tx | from = Just s }) ]


getTempUserErc20Addr model =
    Dict.get userTempErc20AddrId model.fields


setTempUserErc20Addr : String -> Msg
setTempUserErc20Addr =
    SetField userTempErc20AddrId


getBallotTxid : Model -> Maybe String
getBallotTxid model =
    Dict.get txidCheckId model.fields


setBallotTxid : String -> Msg
setBallotTxid =
    SetField txidCheckId


dlgtAddrField : String -> String
dlgtAddrField bHash =
    "delegate.address." ++ toString bHash


getField : String -> Model -> Maybe String
getField k m =
    Dict.get k m.fields


setBoolField : String -> Bool -> Msg
setBoolField =
    SetBoolField


getBoolField : Model -> String -> Maybe Bool
getBoolField model k =
    Dict.get k model.boolFields


getDelegateAddress : Model -> Maybe String
getDelegateAddress model =
    model.currentBallot
        |> Maybe.andThen (\b -> Dict.get (dlgtAddrField b) model.fields)


getEthNodeTemp : Model -> Maybe String
getEthNodeTemp model =
    Dict.get ethNodeTemp model.fields


setEthNodeTemp : String -> Msg
setEthNodeTemp =
    SetField ethNodeTemp


genVoteOptId : String -> Int -> Int
genVoteOptId bHash i =
    hashToInt bHash + i


ballotValToBytes : Int -> Result String RangeBallot3Bits
ballotValToBytes value =
    let
        -- this will change -3 to 0, 0 to +3, and +3 to 6, for example (with ballotRangeAbs = 3)
        absValue =
            value + ballotRangeAbs

        binaryValueString =
            toRadix 2 absValue
                |> Result.map (String.padLeft 3 '0')

        -- this will ensure we only have 1s and 0s in our string due to parseIntRadix
        reencodedOkay =
            binaryValueString
                |> Result.andThen (parseIntRadix 2)
                |> Result.mapError toString
                |> Result.andThen
                    (\val ->
                        if val < 0 || val > ballotMax then
                            Err "Ballot value out of range"
                        else
                            Ok True
                    )

        procString =
            String.split "" >> List.map String.toInt >> Result.Extra.combine >> Result.andThen intsToRangeBallot3Bits
    in
    case reencodedOkay of
        Ok _ ->
            case binaryValueString of
                Ok str ->
                    procString str

                Err err ->
                    Err <| "Error parsing ballot: " ++ toString err

        Err msg ->
            Err <| "Ballot failed reencoding check: " ++ msg


codeSection : List (Html Msg) -> Html Msg
codeSection code =
    pre [ class "ba pa3" ] code


codepointToBinary : Int -> Result ParseInt.Error String
codepointToBinary =
    Result.map (String.padLeft 8 '0') << toRadix 2


toStrDropQts : a -> String
toStrDropQts v =
    let
        str =
            toString v
    in
    if String.left 1 str == "\"" then
        String.dropRight 1 (String.dropLeft 1 str)
    else
        str


formatTsAsDate : Int -> String
formatTsAsDate ts =
    format USDate.config "%b %e %Y, %I:%M %p" <| Date.fromTime <| toFloat <| ts * 1000
