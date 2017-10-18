module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)

import Html exposing (Html, em, span, text)


type alias BallotOption msg =
    { id : Int
    , rSchedule : ReleaseSchedule
    , description : Html msg
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


voteOptions : List (BallotOption msg)
voteOptions =
    [ BallotOption 1337000001 (ReleaseSchedule 8 42) <| text "This is the proposal in the whitepaper. The release will take approximately 1 year."
    , BallotOption 1337000002 (ReleaseSchedule 42 8) <| text "This is like the release schedule in the whitepaper, but smaller chunks will be released more frequently."
    , BallotOption 1337000003 (ReleaseSchedule 16 42) <| text "This release will occur over 2 years."
    , BallotOption 1337000004 (ReleaseSchedule 1 42) <| span [] [ text "This will result in the ", em [] [ text "full" ], text " release of all tokens at 42 days" ]
    ]
