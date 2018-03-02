module SecureVote.SPAs.AdminUI.Fields exposing (..)


democHashId =
    "demoHashId"


selectBallotId : String
selectBallotId =
    "selectBallotElementId"


selectOptionId =
    "selectOptionTypeId"


idB1F s =
    "bBallotV01" ++ s


bTitleId =
    idB1F "Title"


shortDescId =
    idB1F "ShortDesc"


longDescId =
    idB1F "LongDesc"


startTimeId =
    idB1F "StartTime"


endTimeId =
    idB1F "EndTime"


erc20Id =
    idB1F "Erc20"


encPkId =
    idB1F "EncPK"


discussId =
    idB1F "DiscussionLink"


isBindingId =
    idB1F "IsBinding"
