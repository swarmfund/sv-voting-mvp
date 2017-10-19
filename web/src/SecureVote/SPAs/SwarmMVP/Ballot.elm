module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)

import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml(..))


type alias BallotOption msg =
    { id : Int
    , rSchedule : ReleaseSchedule
    , description : DialogHtml msg
    }


type alias ReleaseSchedule =
    { nReleases : Int
    , releaseLength : Int
    }


renderReleaseScheduleTitle : ReleaseSchedule -> String
renderReleaseScheduleTitle { nReleases, releaseLength } =
    let
        sOptional =
            if nReleases > 1 then
                "s"
            else
                ""
    in
    toString nReleases ++ " release" ++ sOptional ++ " of " ++ toString releaseLength ++ " days"


addBallotDesc : List (BallotOption msg) -> List (BallotOption msg)
addBallotDesc ballotOptions =
    let
        expandDesc bOpt =
            bOpt
    in
    List.map expandDesc ballotOptions


voteOptions : List (BallotOption msg)
voteOptions =
    let
        wrapP inner =
            DlogP [ DlogTxt inner ]
    in
    addBallotDesc <|
        [ BallotOption 1337000001 (ReleaseSchedule 8 42) <| wrapP "This is the proposal in the whitepaper. The release will take approximately 1 year."
        , BallotOption 1337000002 (ReleaseSchedule 42 8) <| wrapP "This is like the release schedule in the whitepaper, but smaller chunks will be released more frequently."
        , BallotOption 1337000003 (ReleaseSchedule 16 42) <| wrapP "This release will occur over 2 years."
        , BallotOption 1337000004 (ReleaseSchedule 1 42) <| DlogP [ DlogTxt "This will result in the ", DlogEm "full", DlogTxt " release of all tokens at 42 days" ]
        ]
