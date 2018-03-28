module SecureVote.SPAs.AdminUI.Fields exposing (..)


democHashId =
    "demoHashId"


selectBallotId : String
selectBallotId =
    "selectBallotElementId"


selectOptionId =
    "selectOptionTypeId"


genSimpOptTitleId i =
    idB1F <| "-simpleOpt-title-" ++ toString i


genSimpOptDescId i =
    idB1F <| "-simpleOpt-desc-" ++ toString i


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


rangeVoteNumOptsId =
    idB1F "rangeVoteNumOptsId"


uploadBallotButtonId =
    "uploadBallotButtonId"


showWriteBallotButtonId =
    "showWriteBallotButtonId"


saveJsonLoadingIndicator =
    "saveJsonLoadingIndicator"


uploadBallotLoadingInd =
    "uploadBallotLoadingInd"


writeContractLoadingInd =
    "writeContractLoadingInd"
