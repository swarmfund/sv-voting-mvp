module SecureVote.Ballots.Types exposing (..)


type BallotSpec
    = BVer01 BSpec01Impl
    | BVerFF -- not in use, reserved as NOP


type BallotSpecChoice
    = BChoice01


bSpecChoiceToStr : BallotSpecChoice -> String
bSpecChoiceToStr c =
    case c of
        BChoice01 ->
            "Standard Ballot (v01)"


getTitle : BallotSpec -> String
getTitle b =
    case b of
        BVer01 d ->
            d.ballotTitle

        BVerFF ->
            "INVALID-BALLOT"


type alias BSpec01Impl =
    { ballotTitle : String
    , shortDesc : String
    , longDesc : String
    , startTime : Int
    , endTime : Int
    , erc20Addr : String
    , discussionLink : Maybe String
    , binding : Bool
    , encryptionPK : Maybe String
    , options : OptsOuter
    }


emptyBSpec01 =
    BVer01
        { ballotTitle = ""
        , shortDesc = ""
        , longDesc = ""
        , startTime = 0
        , endTime = 2000000000
        , erc20Addr = ""
        , discussionLink = Nothing
        , binding = True
        , encryptionPK = Nothing
        , options = OptsNothing
        }


type OptsOuter
    = OptsSimple SimpleVer (List SimpleOption)
    | OptsBinary
    | OptsNothing -- not valid, just here as placeholder


optsNOptions : OptsOuter -> Int
optsNOptions os =
    case os of
        OptsSimple RangeVotingPlusMinus3 xs ->
            List.length xs

        OptsBinary ->
            1

        OptsNothing ->
            0


type OptsChoice
    = OChSimpleRange
    | OChBinary


oChoiceToStr : OptsChoice -> String
oChoiceToStr o =
    case o of
        OChSimpleRange ->
            "Range Voting"

        OChBinary ->
            "Binary Yes/No"


type alias SimpleOption =
    { optionTitle : String, optionDesc : Maybe String }


type SimpleVer
    = RangeVotingPlusMinus3
