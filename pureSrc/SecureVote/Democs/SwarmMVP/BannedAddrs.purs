module SecureVote.Democs.SwarmMVP.BannedAddrs where
  
import Prelude

import SecureVote.Democs.SwarmMVP.Types (Voter, toVoter, unsafeToAddress)
  

bannedAddresses :: Array Voter
bannedAddresses = 
    [ toVoter $ unsafeToAddress "0x0000000000000000000000000000000000000000"
    , toVoter $ unsafeToAddress "0x1111111111111111111111111111111111111111"
        -- todo: add Swarm Foundation addresses
    ]
