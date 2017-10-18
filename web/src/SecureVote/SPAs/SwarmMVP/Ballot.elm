module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)


type alias BallotOption =
    { id : Int
    , title : String
    , description : String
    , params : String
    }


reallyLongDescription : String
reallyLongDescription =
    "Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long DescriptionReally long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long DescriptionReally long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description "


voteOptions : List BallotOption
voteOptions =
    [ BallotOption 1337000001 "Option 1" reallyLongDescription ""
    , BallotOption 1337000002 "Option 2" "Description 2" ""
    , BallotOption 1337000003 "Option 3" "Description 3" ""
    , BallotOption 1337000004 "Option 4" "Description 4" ""
    ]
