module SecureVote.Democs.SwarmMVP.BannedAddrs where
  
import Prelude

import SecureVote.Democs.SwarmMVP.Types (Voter, toVoter, unsafeToAddress)
  

bannedAddresses :: Array Voter
bannedAddresses = 
    [ toVoter $ unsafeToAddress "0x0000000000000000000000000000000000000000"
    , toVoter $ unsafeToAddress "0x1111111111111111111111111111111111111111"
        -- todo: add Swarm Foundation addresses
    , toVoter $ unsafeToAddress "0x8Bf7b2D536D286B9c5Ad9d99F608e9E214DE63f0"
    ]
