module SecureVote.SPAs.SwarmMVP.Helpers exposing (..)

import Date
import Date.Extra.Config.Config_en_us as USDate
import Date.Extra.Format exposing (format, isoString)
import Dict
import Html exposing (Html, pre)
import Html.Attributes exposing (class)
import ParseInt exposing (parseIntRadix, toRadix)
import RemoteData exposing (RemoteData(Loading))
import Result.Extra exposing (isOk)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Crypto.Hashing exposing (hashToInt)
import SecureVote.Eth.Types exposing (nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Fields exposing (..)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(SetBoolField, SetField))
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


getUserErc20Addr : Model -> Maybe String
getUserErc20Addr model =
    Dict.get userErc20AddrId model.fields


setUserErc20Addr : String -> Msg
setUserErc20Addr =
    SetField userErc20AddrId


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
